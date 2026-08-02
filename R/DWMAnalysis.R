#' Downed Woody Material Analysis Specification Class
#'
#' @export
setClass("DWMAnalysis", contains = "AnalysisSpec")

#' Create a Downed Woody Material Analysis Specification
#'
#' Construct a specification that reads condition-level downed woody material
#' loadings from `COND_DWM_CALC`. Plot aggregation uses `_UNADJ` fields, while
#' population estimation uses `_ADJ` fields. Fuel, duff, and litter fields are
#' unsuffixed in FIADB and are used as stored in both modes. DWM loadings are
#' already per-acre values and are never multiplied by tree expansion factors.
#'
#' @return A [DWMAnalysis][DWMAnalysis-class] object.
#' @export
#' @examples
#' \dontrun{
#' handler <- eval_handler(con, 501007, spec = dwm_analysis())
#' handler |> aggregate(dwm_cwd(VOLCF))
#' handler |> estimate(dwm_fwd(CARBON, size = "ALL"))
#' }
dwm_analysis <- function() {
  new("DWMAnalysis")
}

#' @noRd
setMethod("initialize_tables", "DWMAnalysis", function(spec, db, evalid, backend = NULL) {
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

  cond_dwm_calc_qry <- tryCatch(
    dplyr::tbl(db, tbl_ref("COND_DWM_CALC")),
    error = function(e) {
      stop(
        "`COND_DWM_CALC` is required for `dwm_analysis()` but is not available.",
        call. = FALSE
      )
    }
  )

  required_columns <- c("EVALID", "PLT_CN", "CONDID")
  missing_columns <- setdiff(required_columns, colnames(cond_dwm_calc_qry))
  if (length(missing_columns) > 0) {
    stop(
      "`COND_DWM_CALC` is missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  cond_dwm_calc_qry <- cond_dwm_calc_qry %>%
    dplyr::filter(EVALID == !!evalid) %>%
    dplyr::semi_join(cond_qry, by = c("PLT_CN", "CONDID"))

  list(
    pop_eval = pop_eval_qry,
    pop_estn_unit = pop_estn_unit_qry,
    pop_stratum = pop_stratum_qry,
    pop_plot_stratum_assgn = pop_plot_stratum_assgn_qry,
    plot = plot_qry,
    cond = cond_qry,
    cond_dwm_calc = cond_dwm_calc_qry
  )
})

#' @noRd
setMethod("aggregate_data", "DWMAnalysis", function(spec, handler, ...) {
  args <- list(...)
  arg_names <- names(args)
  unnamed <- if (is.null(arg_names)) rep(TRUE, length(args)) else arg_names == ""
  named_args <- if (is.null(arg_names)) character(0) else arg_names[!unnamed & nzchar(arg_names)]
  unknown_named <- setdiff(named_args, "sparse")

  if (length(unknown_named) > 0) {
    stop(
      "Unknown named argument(s) for `aggregate()`: ",
      paste(unknown_named, collapse = ", "),
      call. = FALSE
    )
  }
  if (sum(unnamed) != 1) {
    stop("`aggregate()` accepts exactly one scoped target helper per call.", call. = FALSE)
  }

  parsed <- .parse_target_spec(args[[which(unnamed)]], "aggregate")
  sparse <- if ("sparse" %in% names(args)) args[["sparse"]] else FALSE

  if (parsed$slot == "dwm") {
    if (is.null(parsed$dwm_targets)) {
      stop(
        "DWM aggregation requires a component helper wrapped in `dwm()`, such as `dwm(dwm_cwd(VOLCF))`.",
        call. = FALSE
      )
    }
    return(.make_dwm_aggregates(handler, parsed$dwm_targets, sparse = sparse))
  }

  if (parsed$slot == "cond") {
    if (length(parsed$targets) > 0 && !all(parsed$targets == "1")) {
      stop("Only `aggregate(cond())` or `aggregate(cond(1))` is supported for condition aggregation.")
    }
    res <- .make_cond_aggregates(handler, sparse = sparse)
    if (length(parsed$target_names) == 1 && nzchar(parsed$target_names[[1]])) {
      res <- res %>%
        dplyr::rename_with(
          ~ parsed$target_names[[1]],
          dplyr::all_of("prop")
        )
    }
    return(res)
  }

  if (parsed$slot %in% c("tree", "tree_history")) {
    stop(
      "`dwm_analysis()` supports DWM component helpers and `cond()` aggregation, not `",
      parsed$slot,
      "()`.",
      call. = FALSE
    )
  }

  stop("Unsupported slot: ", parsed$slot, call. = FALSE)
})

#' @noRd
setMethod("spec_summary_fields", "DWMAnalysis", function(spec, handler) {
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
