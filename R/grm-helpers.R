#' GRM Survivor Indicator
#'
#' Builds a tree-history expression identifying surviving trees.
#'
#' A record is classified as a survivor when `STATUSCD_begin` matches
#' `begin_status` and `STATUSCD` matches `end_status`.
#'
#' @param begin_status Integer code treated as live at beginning.
#' @param end_status Integer code treated as live at end.
#' @param as_integer Logical. If `TRUE`, returns 0/1 integer indicator.
#' @return An expression to be evaluated in `tree_history` context.
#' @export
#' @examples
#' \dontrun{
#' handler |>
#'   transform(
#'     tree_history(survivor = grm_survivor())
#'   )
#' }
grm_survivor <- function(begin_status = 1L, end_status = 1L, as_integer = TRUE) {
  survivor_expr <- rlang::expr(
    STATUSCD_begin == !!begin_status & STATUSCD == !!end_status
  )

  if (isTRUE(as_integer)) {
    return(rlang::expr(dplyr::if_else(!!survivor_expr, 1L, 0L, missing = 0L)))
  }

  survivor_expr
}

#' GRM Ingrowth Indicator
#'
#' Placeholder helper for ingrowth classification in `tree_history` records.
#'
#' @return No return value; always errors until implemented.
#' @export
grm_ingrowth_live <- function(end_status = 1L, as_integer = TRUE) {
  ingrowth_live_expr <- rlang::expr(
    is.na(PREVDIA) & STATUSCD == !!end_status
  )
  if (isTRUE(as_integer)) {
    return(rlang::expr(dplyr::if_else(!!ingrowth_live_expr, 1L, 0L, missing = 0L)))
  }

  ingrowth_live_expr
}

#' GRM Mortality Indicator
#'
#' Placeholder helper for mortality classification in `tree_history` records.
#'
#' @return No return value; always errors until implemented.
#' @export
grm_mortality <- function(begin_status = 1L, end_status = 2L, as_integer = TRUE) {
  mortality_expr <- rlang::expr(
    STATUSCD_begin == !!begin_status & STATUSCD == !!end_status
  )
  if (isTRUE(as_integer)) {
    return(rlang::expr(dplyr::if_else(!!mortality_expr, 1L, 0L, missing = 0L)))
  }

  mortality_expr
}
