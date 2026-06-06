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
#' @param backend Optional DatabaseBackend for custom schema/table names.
#' @return A list of lazy queries.
#' @export
setGeneric("initialize_tables", function(schema, db, evalid, backend = NULL) standardGeneric("initialize_tables"))

#' @describeIn initialize_tables Initialize tables for Status Analysis
setMethod("initialize_tables", "StatusAnalysis", function(schema, db, evalid, backend = NULL) {
  # Use default backend if none provided
  if (is.null(backend)) {
    backend <- database_backend()
  }
  
  # Helper to get table reference
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

  subp_cond_qry <- dplyr::tbl(db, tbl_ref("SUBP_COND")) %>%
    dplyr::semi_join(cond_qry, by = c("PLT_CN", "CONDID"))

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

#' @describeIn initialize_tables Initialize tables for Change Analysis (Skeleton)
setMethod("initialize_tables", "ChangeAnalysis", function(schema, db, evalid) {
  # TODO: Implement change analysis table loading
  # This will likely involve loading TREE_GRM_COMPONENT, TREE_GRM_MIDPT, etc.
  list()
})

#' Initialize Tables with Custom Schema
#'
#' @param schema An AnalysisSchema object.
#' @param db A DBIConnection object.
#' @param evalid The evaluation ID.
#' @param db_schema Optional database schema name to prefix table names.
#' @param table_names Optional named list mapping standard names to custom table names.
#' @return A list of lazy queries.
#' @export
setGeneric("initialize_tables_custom", function(schema, db, evalid, db_schema = NULL, table_names = list()) {
  standardGeneric("initialize_tables_custom")
})

#' @describeIn initialize_tables_custom Initialize tables for Status Analysis with custom schema
setMethod("initialize_tables_custom", "StatusAnalysis", function(schema, db, evalid, db_schema = NULL, table_names = list()) {
  # Default table names
  defaults <- list(
    pop_eval = "POP_EVAL",
    pop_estn_unit = "POP_ESTN_UNIT",
    pop_stratum = "POP_STRATUM",
    pop_plot_stratum_assgn = "POP_PLOT_STRATUM_ASSGN",
    plot = "PLOT",
    cond = "COND",
    tree = "TREE",
    ref_species = "REF_SPECIES",
    subp_cond = "SUBP_COND"
  )
  
  # Override with user-provided names
  table_names <- utils::modifyList(defaults, table_names)
  
  # Helper to get table reference
  get_table <- function(name) {
    if (!is.null(db_schema)) {
      dbplyr::in_schema(db_schema, name)
    } else {
      name
    }
  }
  
  pop_eval_qry <- dplyr::tbl(db, get_table(table_names$pop_eval)) %>%
    dplyr::filter(EVALID == !!evalid)

  pop_estn_unit_qry <- dplyr::tbl(db, get_table(table_names$pop_estn_unit)) %>%
    dplyr::semi_join(pop_eval_qry, by = c("EVAL_CN" = "CN"))

  pop_stratum_qry <- dplyr::tbl(db, get_table(table_names$pop_stratum)) %>%
    dplyr::semi_join(pop_estn_unit_qry, by = c("ESTN_UNIT_CN" = "CN"))

  pop_plot_stratum_assgn_qry <- dplyr::tbl(db, get_table(table_names$pop_plot_stratum_assgn)) %>%
    dplyr::semi_join(pop_stratum_qry, by = c("STRATUM_CN" = "CN"))

  plot_qry <- dplyr::tbl(db, get_table(table_names$plot)) %>%
    dplyr::semi_join(pop_plot_stratum_assgn_qry, by = c("CN" = "PLT_CN"))

  cond_qry <- dplyr::tbl(db, get_table(table_names$cond)) %>%
    dplyr::semi_join(plot_qry, by = c("PLT_CN" = "CN"))

  tree_qry <- dplyr::tbl(db, get_table(table_names$tree)) %>%
    dplyr::semi_join(cond_qry, by = c("PLT_CN", "CONDID"))

  ref_species_qry <- tryCatch(
    dplyr::tbl(db, get_table(table_names$ref_species)),
    error = function(e) NULL
  )

  subp_cond_qry <- dplyr::tbl(db, get_table(table_names$subp_cond)) %>%
    dplyr::semi_join(cond_qry, by = c("PLT_CN", "CONDID"))

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
