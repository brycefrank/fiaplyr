#' @importFrom dplyr %>%
NULL

utils::globalVariables(c("EVALID", "STATECD", "INVYR", "DIA", "MACRO_BREAKPOINT_DIA", 
                         "ADJ_FACTOR_MICR", "ADJ_FACTOR_SUBP", "ADJ_FACTOR_MACR", 
                         "TPA_UNADJ", "VOLCFNET", "EXPNS"))

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
#' @param verbose Logical. If TRUE (default), shows progress messages via cli.
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
