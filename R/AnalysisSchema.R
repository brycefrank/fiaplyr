#' Analysis Schema Class
#'
#' @export
setClass("AnalysisSchema", contains = "VIRTUAL")

#' Status Analysis Schema
#'
#' @export
setClass("StatusAnalysis", contains = "AnalysisSchema")

#' Change Analysis Schema
#'
#' @export
setClass("ChangeAnalysis", contains = "AnalysisSchema")

#' Initialize Tables
#'
#' @param schema An AnalysisSchema object.
#' @param db A DBIConnection object.
#' @param evalid The evaluation ID.
#' @return A list of lazy queries.
#' @export
setGeneric("initialize_tables", function(schema, db, evalid) standardGeneric("initialize_tables"))

#' @describeIn initialize_tables Initialize tables for Status Analysis
setMethod("initialize_tables", "StatusAnalysis", function(schema, db, evalid) {
  pop_eval_qry <- dplyr::tbl(db, "POP_EVAL") %>%
    dplyr::filter(EVALID == !!evalid)

  # Filter POP_ESTN_UNIT first using EVALID
  pop_estn_unit_qry <- dplyr::tbl(db, "POP_ESTN_UNIT") %>%
    dplyr::semi_join(pop_eval_qry, by = c("EVAL_CN" = "CN"))

  # Filter POP_STRATUM using POP_ESTN_UNIT
  pop_stratum_qry <- dplyr::tbl(db, "POP_STRATUM") %>%
    dplyr::semi_join(pop_estn_unit_qry, by = c("ESTN_UNIT_CN" = "CN"))

  # Filter POP_PLOT_STRATUM_ASSGN using POP_STRATUM
  pop_plot_stratum_assgn_qry <- dplyr::tbl(db, "POP_PLOT_STRATUM_ASSGN") %>%
    dplyr::semi_join(pop_stratum_qry, by = c("STRATUM_CN" = "CN"))

  # Filter PLOT using POP_PLOT_STRATUM_ASSGN
  plot_qry <- dplyr::tbl(db, "PLOT") %>%
    dplyr::semi_join(pop_plot_stratum_assgn_qry, by = c("CN" = "PLT_CN"))

  # Filter COND using PLOT
  cond_qry <- dplyr::tbl(db, "COND") %>%
    dplyr::semi_join(plot_qry, by = c("PLT_CN" = "CN"))

  # Filter TREE using COND
  tree_qry <- dplyr::tbl(db, "TREE") %>%
    dplyr::semi_join(cond_qry, by = c("PLT_CN", "CONDID"))

  # Filter SUBP_COND using COND
  subp_cond_qry <- dplyr::tbl(db, "SUBP_COND") %>%
    dplyr::semi_join(cond_qry, by = c("PLT_CN", "CONDID"))

  list(
    pop_eval = pop_eval_qry,
    pop_estn_unit = pop_estn_unit_qry,
    pop_stratum = pop_stratum_qry,
    pop_plot_stratum_assgn = pop_plot_stratum_assgn_qry,
    plot = plot_qry,
    cond = cond_qry,
    tree = tree_qry,
    subp_cond = subp_cond_qry
  )
})

#' @describeIn initialize_tables Initialize tables for Change Analysis (Skeleton)
setMethod("initialize_tables", "ChangeAnalysis", function(schema, db, evalid) {
  # TODO: Implement change analysis table loading
  # This will likely involve loading TREE_GRM_COMPONENT, TREE_GRM_MIDPT, etc.
  list()
})

#' Aggregate Data
#'
#' @param schema An AnalysisSchema object.
#' @param handler The EvalHandler object.
#' @param ... Arguments for aggregation (formula, sparse, etc.)
#' @return A lazy query with aggregates.
#' @export
setGeneric("aggregate_data", function(schema, handler, ...) standardGeneric("aggregate_data"))

#' @describeIn aggregate_data Aggregate data for Status Analysis
setMethod("aggregate_data", "StatusAnalysis", function(schema, handler, ...) {
  args <- list(...)
  if (length(args) == 0 || !inherits(args[[1]], "formula")) {
    stop("Must provide a formula as the first argument (e.g., tree ~ VOLCFGRS).")
  }

  formula <- args[[1]]
  sparse <- if ("sparse" %in% names(args)) args[["sparse"]] else FALSE

  parsed <- parse_formula(formula)
  slot_name <- parsed$slot
  targets <- parsed$targets

  if (slot_name == "tree") {
    if (length(targets) == 1 && targets == "1") {
      return(.make_tree_aggregates(handler, sparse = sparse))
    }
    syms <- rlang::syms(targets)
    return(.make_tree_aggregates(handler, !!!syms, sparse = sparse))
  } else if (slot_name == "cond") {
    if (!all(targets == "1")) {
      stop("Only 'cond ~ 1' is currently supported for condition aggregation.")
    }
    return(.make_cond_aggregates(handler, sparse = sparse))
  } else {
    stop("Unsupported slot: ", slot_name)
  }
})

#' @describeIn aggregate_data Aggregate data for Change Analysis
setMethod("aggregate_data", "ChangeAnalysis", function(schema, handler, ...) {
   args <- list(...)
   # formula <- args[[1]]
   # Here we would implement the logic for b(), m(), e()

   stop("Change analysis aggregation not yet implemented.")
})
