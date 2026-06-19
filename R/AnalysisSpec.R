#' Analysis Specification Base Class
#'
#' @export
setClass("AnalysisSpec", contains = "VIRTUAL")

#' Change Analysis Spec
#'
#' @export
setClass("ChangeAnalysis", contains = "AnalysisSpec")

#' Initialize Tables for an Analysis Spec
#'
#' @param spec An AnalysisSpec object.
#' @param db A DBIConnection object.
#' @param evalid The evaluation ID.
#' @param backend Optional DatabaseMapping for custom schema/table names.
#' @return A list of lazy queries.
#' @export
setGeneric("initialize_tables", function(spec, db, evalid, backend = NULL) {
  standardGeneric("initialize_tables")
})

#' Aggregate Data for an Analysis Spec
#'
#' @param spec An AnalysisSpec object.
#' @param handler The EvalHandler object.
#' @param ... Arguments for aggregation.
#' @return A lazy query.
#' @export
setGeneric("aggregate_data", function(spec, handler, ...) {
  standardGeneric("aggregate_data")
})

#' Build Spec-Specific Summary Fields
#'
#' @param spec An AnalysisSpec object.
#' @param handler The EvalHandler object.
#' @return A named list of summary fields.
#' @export
methods::setGeneric("spec_summary_fields", function(spec, handler) {
  methods::standardGeneric("spec_summary_fields")
}, where = topenv())

#' @describeIn spec_summary_fields Default summary fields for AnalysisSpec
methods::setMethod("spec_summary_fields", "AnalysisSpec", function(spec, handler) {
  list()
})