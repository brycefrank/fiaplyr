#' Aggregate Data to Plot Level
#'
#' @param handler A handler object.
#' @param ... Additional arguments.
#' @importFrom stats aggregate
#' @export
setGeneric("aggregate", function(handler, ...) standardGeneric("aggregate"))

#' Aggregate Trees to Plot Level
#'
#' @param object A handler object.
#' @param ... Additional arguments.
#' @export
setGeneric("aggregate_tree", function(object, ...) standardGeneric("aggregate_tree"))

#' Aggregate Conditions to Plot Level
#'
#' @param object A handler object.
#' @param ... Additional arguments.
#' @export
setGeneric("aggregate_cond", function(object, ...) standardGeneric("aggregate_cond"))

#' @export
setGeneric("mutate_tree", function(handler, ...) standardGeneric("mutate_tree"))

#' @export
setGeneric("mutate_cond", function(handler, ...) standardGeneric("mutate_cond"))

#' @export
setGeneric("filter_tree", function(handler, ...) standardGeneric("filter_tree"))

#' @export
setGeneric("filter_cond", function(handler, ...) standardGeneric("filter_cond"))

#' @export
setGeneric("set_tree_domains", function(.data, ...) standardGeneric("set_tree_domains"))

#' @export
setGeneric("set_cond_domains", function(.data, ...) standardGeneric("set_cond_domains"))

#' Get Strata Weights
#'
#' @param handler A handler object.
#' @return A lazy query with strata weights.
#' @export
setGeneric("get_strata_weights", function(handler) standardGeneric("get_strata_weights"))

#' Estimate Population Parameters
#'
#' @param object An estimator object.
#' @param ... One or more formulas specifying estimation targets.
#' @param output Output scale, either "mean" (default) or "total".
#' @param margins Logical. If `TRUE`, returns all marginal estimates in addition
#'   to the full cross-domain estimates. Marginals are produced by re-running
#'   the estimation pipeline for every strict subset of the active domain
#'   variables, including the grand total (no domains). Dropped domain columns
#'   appear as `NA` in the output, indicating aggregation over all values of
#'   that variable. Defaults to `FALSE`.
#' @export
setGeneric("estimate", function(object, ..., output = "mean", margins = FALSE) {
	standardGeneric("estimate")
})
