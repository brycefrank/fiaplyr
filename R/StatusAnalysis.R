#' Status Analysis Spec
#'
#' @export
setClass("StatusAnalysis", contains = "AnalysisSpec")

#' Initialize Tables for Status Analysis
#'
#' @param schema A StatusAnalysis object.
#' @param db A DBIConnection object.
#' @param evalid The evaluation ID.
#' @param backend Optional DatabaseMapping for custom schema/table names.
#' @return A list of lazy queries.
#' @export
setMethod("initialize_tables", "StatusAnalysis", function(schema, db, evalid, backend = NULL) {
  if (is.null(backend)) {
    backend <- database_backend()
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

  subp_cond_qry <- dplyr::tbl(db, tbl_ref("SUBP_COND")) %>%
    dplyr::semi_join(cond_qry, by = c("PLT_CN", "CONDID"))

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
#' @param schema A StatusAnalysis object.
#' @param handler The EvalHandler object.
#' @param ... Arguments for aggregation (scoped target helper, sparse, etc.)
#' @return A lazy query with aggregates.
#' @export
setMethod("aggregate_data", "StatusAnalysis", function(schema, handler, ...) {
  args <- list(...)
  arg_names <- names(args)
  unnamed <- if (is.null(arg_names)) rep(TRUE, length(args)) else arg_names == ""

  if (!any(unnamed)) {
    stop("Must provide exactly one scoped target helper such as `tree(VOLCFGRS)` or `cond()`.")
  }

  if (sum(unnamed) != 1) {
    stop("`aggregate()` accepts exactly one scoped target helper per call.")
  }

  spec <- args[[which(unnamed)]]
  sparse <- if ("sparse" %in% names(args)) args[["sparse"]] else FALSE

  parsed <- .parse_target_spec(spec, "aggregate")
  slot_name <- parsed$slot
  targets <- parsed$targets

  if (slot_name == "tree") {
    if (length(targets) == 0 || (length(targets) == 1 && targets == "1")) {
      return(.make_tree_aggregates(handler, sparse = sparse))
    }
    syms <- rlang::syms(targets)
    return(.make_tree_aggregates(handler, !!!syms, sparse = sparse))
  } else if (slot_name == "cond") {
    if (length(targets) > 0 && !all(targets == "1")) {
      stop("Only `aggregate(cond())` or `aggregate(cond(1))` is currently supported for condition aggregation.")
    }
    return(.make_cond_aggregates(handler, sparse = sparse))
  } else {
    stop("Unsupported slot: ", slot_name)
  }
})