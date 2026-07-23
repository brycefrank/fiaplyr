#' Status Analysis Specification Class
#'
#' @export
setClass("StatusAnalysis", contains = "AnalysisSpec")

#' Create a Status Analysis Specification
#'
#' Construct a [StatusAnalysis][StatusAnalysis-class] object for use with
#' [eval_handler()][eval_handler]. Status analysis is a specification meant to
#' support the estimation of the current status of, particularly, tree-
#' and condition-oriented attributes. In contrast,
#' [grm_analysis()][grm_analysis] is a specification meant to support the
#' estimation of growth, removals, and mortality (GRM) attributes. Most standard
#' population parameters can be estimated under this specification. Therefore,
#' it is the default specification for [eval_handler()][eval_handler].
#' Generally users should seek to employ evaluations ending with `01`,
#' indicating an evaluation engineered for status analysis, but this is not
#' strictly enforced.
#'
#' @return A [StatusAnalysis][StatusAnalysis-class] object.
#' @export
#'
#' @examples
#' \dontrun{
#' handler <- eval_handler(con, 501103, spec = status_analysis())
#' }
status_analysis <- function() {
  new("StatusAnalysis")
}

#' Initialize Tables for Status Analysis
#'
#' @param spec A StatusAnalysis object.
#' @param db A DBIConnection object.
#' @param evalid The evaluation ID.
#' @param backend Optional DatabaseMapping for custom schema/table names.
#' @return A list of lazy queries.
#' @noRd
setMethod("initialize_tables", "StatusAnalysis", function(spec, db, evalid, backend = NULL) {
  if (is.null(backend)) {
    backend <- database_mapping()
  }

  tbl_ref <- function(name) get_table_ref(backend, name)

  pop_eval_qry <- dplyr::tbl(db, tbl_ref("POP_EVAL")) %>%
    dplyr::filter(EVALID == !!evalid)

  pop_estn_unit_qry <- dplyr::tbl(db, tbl_ref("POP_ESTN_UNIT")) %>%
    dplyr::semi_join(pop_eval_qry, by = c("EVAL_CN" = "CN"))

  pop_stratum_qry <- dplyr::tbl(db, tbl_ref("POP_STRATUM")) %>%
    dplyr::semi_join(pop_estn_unit_qry, by = c("ESTN_UNIT_CN" = "CN"))

  pop_plot_stratum_assgn_qry <- dplyr::tbl(db, tbl_ref("POP_PLOT_STRATUM_ASSGN")) %>%
    dplyr::semi_join(pop_stratum_qry, by = c("STRATUM_CN" = "CN"))

  plot_qry <- dplyr::tbl(db, tbl_ref("PLOT")) %>%
    dplyr::semi_join(pop_plot_stratum_assgn_qry, by = c("CN" = "PLT_CN"))

  cond_qry <- dplyr::tbl(db, tbl_ref("COND")) %>%
    dplyr::semi_join(plot_qry, by = c("PLT_CN" = "CN"))

  tree_qry <- dplyr::tbl(db, tbl_ref("TREE")) %>%
    dplyr::semi_join(cond_qry, by = c("PLT_CN", "CONDID"))

  ref_species_qry <- tryCatch(
    dplyr::tbl(db, tbl_ref("REF_SPECIES")),
    error = function(e) NULL
  )

  subp_cond_qry <- tryCatch(
    dplyr::tbl(db, tbl_ref("SUBP_COND")) %>%
      dplyr::semi_join(cond_qry, by = c("PLT_CN", "CONDID")),
    error = function(e) NULL
  )

  list(
    pop_eval = pop_eval_qry,
    pop_estn_unit = pop_estn_unit_qry,
    pop_stratum = pop_stratum_qry,
    pop_plot_stratum_assgn = pop_plot_stratum_assgn_qry,
    plot = plot_qry,
    cond = cond_qry,
    tree = tree_qry,
    ref_species = ref_species_qry,
    subp_cond = subp_cond_qry
  )
})

#' Aggregate Data for Status Analysis
#'
#' @param spec A StatusAnalysis object.
#' @param handler The EvalHandler object.
#' @param ... Arguments for aggregation (scoped target helper, sparse, etc.)
#' @return A lazy query with aggregates.
#' @noRd
setMethod("aggregate_data", "StatusAnalysis", function(spec, handler, ...) {
  args <- list(...)
  arg_names <- names(args)
  unnamed <- if (is.null(arg_names)) rep(TRUE, length(args)) else arg_names == ""

  named_args <- if (is.null(arg_names)) character(0) else arg_names[!unnamed & nzchar(arg_names)]
  unknown_named <- setdiff(named_args, "sparse")
  if (length(unknown_named) > 0) {
    stop("Unknown named argument(s) for `aggregate()`: ", paste(unknown_named, collapse = ", "), call. = FALSE)
  }

  if (!any(unnamed)) {
    stop("Must provide exactly one scoped target helper such as `tree(VOLCFGRS)` or `cond()`.")
  }

  if (sum(unnamed) != 1) {
    stop("`aggregate()` accepts exactly one scoped target helper per call.")
  }

  target_spec <- args[[which(unnamed)]]
  sparse <- if ("sparse" %in% names(args)) args[["sparse"]] else FALSE

  parsed <- .parse_target_spec(target_spec, "aggregate")
  slot_name <- parsed$slot
  targets <- parsed$targets
  target_names <- parsed$target_names
  target_quos <- parsed$quosures

  if (slot_name == "tree") {
    if (length(targets) == 0 || (length(targets) == 1 && targets == "1")) {
      return(.make_tree_aggregates(handler, sparse = sparse))
    }
    if (is.null(target_quos)) {
      target_quos <- rlang::syms(targets)
    }
    return(.make_tree_aggregates(handler, !!!target_quos, sparse = sparse))
  } else if (slot_name == "cond") {
    if (length(targets) > 0 && !all(targets == "1")) {
      stop("Only `aggregate(cond())` or `aggregate(cond(1))` is currently supported for condition aggregation.")
    }
    res <- .make_cond_aggregates(handler, sparse = sparse)
    if (length(target_names) == 1 && nzchar(target_names[[1]])) {
      res <- res %>% dplyr::rename(!!target_names[[1]] := prop)
    }
    return(res)
  } else {
    stop("Unsupported slot: ", slot_name)
  }
})

#' @describeIn spec_summary_fields StatusAnalysis-specific summary fields
#' @noRd
setMethod("spec_summary_fields", "StatusAnalysis", function(spec, handler) {
  plot_stats <- handler@tables$plot %>%
    dplyr::summarise(
      min_invyr = min(INVYR, na.rm = TRUE),
      max_invyr = max(INVYR, na.rm = TRUE),
      min_meas = min(MEASYEAR, na.rm = TRUE),
      max_meas = max(MEASYEAR, na.rm = TRUE)
    ) %>%
    dplyr::collect()

  list(
    min_invyr = plot_stats$min_invyr,
    max_invyr = plot_stats$max_invyr,
    min_meas = plot_stats$min_meas,
    max_meas = plot_stats$max_meas
  )
})
