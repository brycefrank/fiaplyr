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
setMethod("build_tables", "DWMAnalysis", function(spec, plot_qry, db, backend = NULL, evalid = NULL) {
  if (is.null(backend)) {
    backend <- database_mapping()
  }

  if (is.null(evalid)) {
    rlang::abort(
      paste0(
        "`dwm_analysis()` requires an evaluation context because `COND_DWM_CALC` ",
        "is keyed by `EVALID`. Use `eval_handler(con, evalid = ..., spec = dwm_analysis())` ",
        "instead of a `window_handler()`."
      ),
      class = "fiaplyr_dwm_requires_evalid"
    )
  }

  tbl_ref <- function(name) get_table_ref(backend, name)

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
    plot = plot_qry,
    cond = cond_qry,
    cond_dwm_calc = cond_dwm_calc_qry
  )
})

#' @noRd
setMethod("aggregate_data", "DWMAnalysis", function(spec, handler, ...) {
  prep <- .aggregate_prepare(list(...), spec)
  .aggregate_combined(handler, prep$parsed_list, prep$sparse)
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
