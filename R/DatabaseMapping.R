#' Database Mapping Class
#'
#' Handles database-specific schema and table naming conventions.
#'
#' @slot schema_name Optional schema/catalog name prefix
#' @slot table_map Named list mapping standard table names to database-specific names
#' @export
setClass("DatabaseMapping",
  slots = list(
    schema_name = "character",
    table_map = "list"
  )
)

#' Create a Database Mapping
#'
#' A database mapping enables the use of alternative schema and table names,
#' and is passed to the [EvalHandler][EvalHandler-class]. When interacting with
#' a standard FIADB, a mapping is not necessary. However, if your database uses
#' different schema and table names, then a database mapping can be used to
#' construct the handler. Users provide a schema name and an optional list that
#' maps standard table names to custom table names, with the standard names as
#' keys. `fiaplyr` only interacts with a handful of tables, see the example for
#' the entire set.
#'
#' @param schema_name Optional schema name (e.g., "MY_SCHMEMA_NAME")
#' @param table_map Named list to override default table names
#' @return A [DatabaseMapping][DatabaseMapping-class] object
#'
#' @examples
#' custom_mapping <- database_mapping(
#'   schema_name = "MY_SCHEMA",
#'   table_map = list(
#'     POP_EVAL = "MY_POP_EVAL",
#'     POP_ESTN_UNIT = "MY_POP_ESTN_UNIT",
#'     POP_STRATUM = "MY_POP_STRATUM",
#'     POP_PLOT_STRATUM_ASSGN = "MY_POP_PLOT_STRAT",
#'     PLOT = "MY_PLOT",
#'     COND = "MY_COND",
#'     TREE = "MY_TREE",
#'     REF_SPECIES = "MY_REF_SPECIES",
#'     SUBP_COND = "MY_SUBP_COND"
#'   )
#' )
#'
#' @export
database_mapping <- function(schema_name = NULL, table_map = list()) {
  new("DatabaseMapping",
    schema_name = if (is.null(schema_name)) character(0) else schema_name,
    table_map = table_map
  )
}

#' Get Table Reference
#'
#' @param backend A DatabaseMapping object
#' @param standard_name The standard FIA table name
#' @return A table reference (string or in_schema object)
#' @noRd
setGeneric("get_table_ref", function(backend, standard_name) standardGeneric("get_table_ref"))

#' @describeIn get_table_ref Get table reference for DatabaseMapping
#' @noRd
setMethod("get_table_ref", "DatabaseMapping", function(backend, standard_name) {
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