#' @import methods
#' @importClassesFrom DBI DBIConnection
#' @importFrom dplyr %>%
NULL

utils::globalVariables(c(
  ".adj_factor",
  ".expander_wt",
  "ADJ_SUBPTYPE",
  "ADJ_FACTOR", "ADJ_FACTOR_MACR", "ADJ_FACTOR_MICR", "ADJ_FACTOR_SUBP",
  "AREA_USED", "CN", "COMPONENT", "CONDPROP_UNADJ", "DIA", "ESTN_UNIT_CN", "EVALID",
  "EVAL_DESCR", "EXPNS", "INVYR", "MACRO_BREAKPOINT_DIA", "MEASYEAR", "P1PNTCNT_EU",
  "P1POINTCNT", "P2POINTCNT", "PLT_CN", "REMPER", "STATECD", "STRATUM_CN", "SUBPTYP_GRM", "TPA_UNADJ", "TPA_UNADJ_begin", "transition",
  "VOLCFNET", "cov_col", "cov_val", "estimate_d", "estimate_n", "eu_area", "n",
  "n_h", "se", "se_d", "se_n", "var", "var_ratio", "var_raw", "var_val", "w_eu",
  "w_h"
))

# Standard column key vectors used across aggregation and estimation.
# .plot_keys_raw uses "CN" (the plot table's primary key before renaming to PLT_CN).
# .plot_keys uses "PLT_CN" (the foreign key name used after joining to strata/estimation).
.plot_keys_raw <- c("CN", "STATECD", "COUNTYCD", "INVYR", "PLOT")
.plot_keys <- c("PLT_CN", "STATECD", "COUNTYCD", "INVYR", "PLOT")
.strat_keys <- c("STRATUM_CN", "ESTN_UNIT_CN", "EVAL_CN", "w_h", "n_h", "n")

#' Get Adjustment Factor Expression
#' 
#' @return A quosure for the adjustment factor case logic.
#' @noRd
get_adj_factor_expr <- function() {
  rlang::expr(
    dplyr::case_when(
      is.na(DIA) ~ ADJ_FACTOR_SUBP,
      DIA < 5.0 ~ ADJ_FACTOR_MICR,
      DIA < dplyr::coalesce(MACRO_BREAKPOINT_DIA, 9999.0) ~ ADJ_FACTOR_SUBP,
      TRUE ~ ADJ_FACTOR_MACR
    )
  )
}

#' Set fiaplyr verbosity
#'
#' Controls whether long-running operations show progress messages.
#'
#' @param verbose Logical. If TRUE (default), shows progress messages.
#' @export
set_fiaplyr_verbosity <- function(verbose = TRUE) {
  options(fiaplyr.verbose = verbose)
}

#' Check if verbose mode is on
#'
#' @return Logical
#' @noRd
is_verbose <- function() {
  getOption("fiaplyr.verbose", default = TRUE)
}

#' Path to packaged Vermont mini FIADB DuckDB
#'
#' Returns the installed path to the DuckDB file distributed with `fiaplyr`.
#'
#' @param mustWork Logical. If TRUE (default), throws an error when the file
#'   is not found.
#' @return A length-1 character vector containing the file path.
#' @export
fiadb_vt_mini_path <- function(mustWork = TRUE) {
  path <- system.file("fiadb_vt_mini.duckdb", package = "fiaplyr")

  if (mustWork && identical(path, "")) {
    stop("Could not find packaged file 'fiadb_vt_mini.duckdb' in fiaplyr.", call. = FALSE)
  }

  path
}
