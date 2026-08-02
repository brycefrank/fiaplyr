#' Status Analysis Specification Class
#'
#' @export
setClass("StatusAnalysis", contains = "AnalysisSpec")

#' Create a Status Analysis Specification
#'
#' Construct a [StatusAnalysis][StatusAnalysis-class] object for use with
#' [eval_handler()][eval_handler]. Status analysis is a specification meant to
#' support the estimation of the current status of, particularly, tree-
#' and condition-oriented attributes. In contrast,
#' [grm_analysis()][grm_analysis] is a specification meant to support the
#' estimation of growth, removals, and mortality (GRM) attributes. Most standard
#' population parameters can be estimated under this specification. Therefore,
#' it is the default specification for [eval_handler()][eval_handler].
#' Generally users should seek to employ evaluations ending with `01`,
#' indicating an evaluation engineered for status analysis, but this is not
#' strictly enforced.
#'
#' Internally, this analysis specification builds lazy `dplyr` queries for
#' evaluation, estimation unit, stratum, plot, condition, and tree tables,
#' restricting them to the relevant selection (typically an evalid). Queries
#' stored within the handler are used to produce plot-level aggregates, which
#' are fed into estimators. Users do not typically interact with the
#' specification, although it plays an important role as a layer between the
#' handler API and the underlying database tables.
#'
#' @return A [StatusAnalysis][StatusAnalysis-class] object.
#' @export
#'
#' @examples
#' \dontrun{
#' handler <- eval_handler(con, 501103, spec = status_analysis())
#' }
status_analysis <- function() {
  new("StatusAnalysis")
}

#' Initialize Tables for Status Analysis
#'
#' @param spec A StatusAnalysis object.
#' @param db A DBIConnection object.
#' @param evalid The evaluation ID.
#' @param backend Optional DatabaseMapping for custom schema/table names.
#' @return A list of lazy queries.
#' @noRd
setMethod("initialize_tables", "StatusAnalysis", function(spec, db, evalid, backend = NULL) {
  if (is.null(backend)) {
    backend <- database_mapping()
  }

  tbl_ref <- function(name) get_table_ref(backend, name)

  pop_eval_qry <- dplyr::tbl(db, tbl_ref("POP_EVAL")) %>%
    dplyr::filter(EVALID == !!evalid)

  pop_estn_unit_qry <- dplyr::tbl(db, tbl_ref("POP_ESTN_UNIT")) %>%
    dplyr::semi_join(pop_eval_qry, by = c("EVAL_CN" = "CN"))

  pop_stratum_qry <- dplyr::tbl(db, tbl_ref("POP_STRATUM")) %>%
    dplyr::semi_join(pop_estn_unit_qry, by = c("ESTN_UNIT_CN" = "CN"))

  pop_plot_stratum_assgn_qry <- dplyr::tbl(db, tbl_ref("POP_PLOT_STRATUM_ASSGN")) %>%
    dplyr::semi_join(pop_stratum_qry, by = c("STRATUM_CN" = "CN"))

  plot_qry <- dplyr::tbl(db, tbl_ref("PLOT")) %>%
    dplyr::semi_join(pop_plot_stratum_assgn_qry, by = c("CN" = "PLT_CN"))

  cond_qry <- dplyr::tbl(db, tbl_ref("COND")) %>%
    dplyr::semi_join(plot_qry, by = c("PLT_CN" = "CN"))

  tree_qry <- dplyr::tbl(db, tbl_ref("TREE")) %>%
    dplyr::semi_join(cond_qry, by = c("PLT_CN", "CONDID"))

  ref_species_qry <- tryCatch(
    dplyr::tbl(db, tbl_ref("REF_SPECIES")),
    error = function(e) NULL
  )

  subp_cond_qry <- tryCatch(
    dplyr::tbl(db, tbl_ref("SUBP_COND")) %>%
      dplyr::semi_join(cond_qry, by = c("PLT_CN", "CONDID")),
    error = function(e) NULL
  )

  list(
    pop_eval = pop_eval_qry,
    pop_estn_unit = pop_estn_unit_qry,
    pop_stratum = pop_stratum_qry,
    pop_plot_stratum_assgn = pop_plot_stratum_assgn_qry,
    plot = plot_qry,
    cond = cond_qry,
    tree = tree_qry,
    ref_species = ref_species_qry,
    subp_cond = subp_cond_qry
  )
})

#' Aggregate Data for Status Analysis
#'
#' @param spec A StatusAnalysis object.
#' @param handler The EvalHandler object.
#' @param ... Arguments for aggregation (scoped target helper, sparse, etc.)
#' @return A lazy query with aggregates.
#' @noRd
setMethod("aggregate_data", "StatusAnalysis", function(spec, handler, ...) {
  prep <- .aggregate_prepare(list(...), spec)
  .aggregate_combined(handler, prep$parsed_list, prep$sparse)
})

#' @describeIn spec_summary_fields StatusAnalysis-specific summary fields
#' @noRd
setMethod("spec_summary_fields", "StatusAnalysis", function(spec, handler) {
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
