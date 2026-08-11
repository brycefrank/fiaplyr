#' Analysis Specification Base Class
#'
#' @export
setClass("AnalysisSpec", contains = "VIRTUAL")

#' Change Analysis Spec
#'
#' @export
setClass("ChangeAnalysis", contains = "AnalysisSpec")

#' Build Analysis Tables for an Analysis Spec
#'
#' Given the handler-selected plot query, build the child tables the aggregation
#' policy needs (cond/tree for status; tree_history for GRM; cond_dwm_calc for
#' DWM). Plot selection is the handler's concern; building the analysis
#' data structure is the spec's.
#'
#' @param spec An AnalysisSpec object.
#' @param plot_qry A lazy plot query restricted to the selected plots.
#' @param db A DBIConnection object.
#' @param backend Optional DatabaseMapping for custom schema/table names.
#' @param evalid The evaluation ID, or `NULL` when the handler has no evaluation
#'   context (e.g. a `WindowHandler`). Specs that are keyed by evaluation
#'   (e.g. DWM) should abort with an explanatory error when it is `NULL`.
#' @return A list of lazy queries, including `plot` and any child tables.
#' @noRd
setGeneric(
  "build_tables",
  function(spec, plot_qry, db, backend = NULL, evalid = NULL) {
    standardGeneric("build_tables")
  }
)

#' Aggregate Data for an Analysis Spec
#'
#' @param spec An AnalysisSpec object.
#' @param handler The EvalHandler object.
#' @param ... Arguments for aggregation.
#' @return A lazy query.
#' @noRd
setGeneric("aggregate_data", function(spec, handler, ...) {
  standardGeneric("aggregate_data")
})

#' Build Spec-Specific Summary Fields
#'
#' @param spec An AnalysisSpec object.
#' @param handler The EvalHandler object.
#' @return A named list of summary fields.
#' @noRd
setGeneric("spec_summary_fields", function(spec, handler) {
  standardGeneric("spec_summary_fields")
})

#' @describeIn spec_summary_fields Default summary fields for AnalysisSpec
#' @noRd
methods::setMethod("spec_summary_fields", "AnalysisSpec", function(spec, handler) {
  list()
})