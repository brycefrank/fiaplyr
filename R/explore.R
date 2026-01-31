#' Explore Available Evaluations
#'
#' Lists all available evaluations in the database with their descriptions.
#'
#' @param db A DBIConnection object connected to an FIA database.
#' @return A tibble containing `EVALID` and `EVAL_DESCR`.
#' @importFrom dplyr tbl select collect arrange
#' @export
explore_evals <- function(db) {
  dplyr::tbl(db, "POP_EVAL") %>%
    dplyr::select(EVALID, EVAL_DESCR) %>%
    dplyr::collect() %>%
    dplyr::arrange(EVALID)
}
