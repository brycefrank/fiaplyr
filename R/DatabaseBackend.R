#' Database Backend Class
#'
#' Handles database-specific schema and table naming conventions.
#'
#' @slot schema_name Optional schema/catalog name prefix
#' @slot table_map Named list mapping standard table names to database-specific names
#' @export
setClass("DatabaseBackend",
  slots = list(
    schema_name = "character",
    table_map = "list"
  )
)

#' Create a Database Backend
#'
#' @param schema_name Optional schema/catalog name (e.g., "fia_2024")
#' @param table_map Named list to override default table names
#' @return A DatabaseBackend object
#' @export
database_backend <- function(schema_name = NULL, table_map = list()) {
  new("DatabaseBackend",
    schema_name = if (is.null(schema_name)) character(0) else schema_name,
    table_map = table_map
  )
}

#' Get Table Reference
#'
#' @param backend A DatabaseBackend object
#' @param standard_name The standard FIA table name
#' @return A table reference (string or in_schema object)
#' @export
setGeneric("get_table_ref", function(backend, standard_name) standardGeneric("get_table_ref"))

#' @describeIn get_table_ref Get table reference for DatabaseBackend
setMethod("get_table_ref", "DatabaseBackend", function(backend, standard_name) {
  # Use custom name if provided, otherwise use standard name
  table_name <- if (is.null(backend@table_map[[standard_name]])) {
    standard_name
  } else {
    backend@table_map[[standard_name]]
  }
  
  # Apply schema prefix if provided
  if (length(backend@schema_name) > 0) {
    dbplyr::in_schema(backend@schema_name, table_name)
  } else {
    table_name
  }
})