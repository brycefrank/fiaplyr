#' Explore Available Evaluations
#'
#' Lists all available evaluations in the database with their descriptions.
#'
#' @param db A DBIConnection object connected to an FIA database.
#' @param backend Optional DatabaseMapping for custom schema/table names.
#' @return A tibble containing `EVALID` and `EVAL_DESCR`.
#' @importFrom dplyr tbl select collect arrange
#' @export
explore_evals <- function(db, backend = NULL) {
  # Use default backend if none provided
  if (is.null(backend)) {
    backend <- database_backend()
  }
  
  # Get table reference
  tbl_ref <- get_table_ref(backend, "POP_EVAL")
  
  dplyr::tbl(db, tbl_ref) %>%
    dplyr::select(EVALID, EVAL_DESCR) %>%
    dplyr::collect() %>%
    dplyr::arrange(EVALID)
}
