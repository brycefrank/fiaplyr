#' Analysis Spec Class
#'
#' @export
setClass("AnalysisSpec", contains = "VIRTUAL")

#' Change Analysis Spec
#'
#' @export
setClass("ChangeAnalysis", contains = "AnalysisSpec")

#' Initialize Tables
#'
#' @param schema An AnalysisSpec object.
#' @param db A DBIConnection object.
#' @param evalid The evaluation ID.
#' @param backend Optional DatabaseMapping for custom schema/table names.
#' @return A list of lazy queries.
#' @export
setGeneric("initialize_tables", function(schema, db, evalid, backend = NULL) standardGeneric("initialize_tables"))

#' @describeIn initialize_tables Initialize tables for Change Analysis (Skeleton)
setMethod("initialize_tables", "ChangeAnalysis", function(schema, db, evalid) {
  # TODO: Implement change analysis table loading
  # This will likely involve loading TREE_GRM_COMPONENT, TREE_GRM_MIDPT, etc.
  list()
})

#' Aggregate Data
#'
#' @param schema An AnalysisSpec object.
#' @param handler The EvalHandler object.
#' @param ... Arguments for aggregation (scoped target helper, sparse, etc.)
#' @return A lazy query with aggregates.
#' @export
setGeneric("aggregate_data", function(schema, handler, ...) standardGeneric("aggregate_data"))

#' @describeIn aggregate_data Aggregate data for Status Analysis
setMethod("aggregate_data", "StatusAnalysis", function(schema, handler, ...) {
  args <- list(...)
  arg_names <- names(args)
  unnamed <- if (is.null(arg_names)) rep(TRUE, length(args)) else arg_names == ""

  if (!any(unnamed)) {
    stop("Must provide exactly one scoped target helper such as `tree(VOLCFGRS)` or `cond()`.")
  }

  if (sum(unnamed) != 1) {
    stop("`aggregate()` accepts exactly one scoped target helper per call.")
  }

  spec <- args[[which(unnamed)]]
  sparse <- if ("sparse" %in% names(args)) args[["sparse"]] else FALSE

  parsed <- .parse_target_spec(spec, "aggregate")
  slot_name <- parsed$slot
  targets <- parsed$targets
  target_names <- parsed$target_names

  if (slot_name == "tree") {
    if (length(targets) == 0 || (length(targets) == 1 && targets == "1")) {
      return(.make_tree_aggregates(handler, sparse = sparse))
    }
    quos <- if (!is.null(parsed$quosures)) {
      parsed$quosures
    } else {
      qs <- rlang::syms(targets)
      names(qs) <- target_names
      qs
    }
    return(.make_tree_aggregates(handler, !!!quos, sparse = sparse))
  } else if (slot_name == "cond") {
    if (length(targets) > 0 && !all(targets == "1")) {
      stop("Only `aggregate(cond())` or `aggregate(cond(1))` is currently supported for condition aggregation.")
    }

    res <- .make_cond_aggregates(handler, sparse = sparse)
    has_named_prop <- length(targets) > 0 && all(targets == "1") && length(target_names) == 1 && nzchar(target_names[[1]])
    if (has_named_prop) {
      res <- res %>% dplyr::rename(!!target_names[[1]] := prop)
    }

    return(res)
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
