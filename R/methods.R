#' Aggregate Data to Plot Level
#'
#' @param x A handler object.
#' @param ... Additional arguments.
#' @importFrom stats aggregate
#' @export
setGeneric("aggregate")

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
setGeneric("mutate_tree", function(.data, ...) standardGeneric("mutate_tree"))

#' @export
setGeneric("mutate_cond", function(.data, ...) standardGeneric("mutate_cond"))

#' @export
setGeneric("filter_tree", function(.data, ...) standardGeneric("filter_tree"))

#' @export
setGeneric("filter_cond", function(.data, ...) standardGeneric("filter_cond"))

#' @export
setGeneric("set_tree_domains", function(.data, ...) standardGeneric("set_tree_domains"))

#' @export
setGeneric("set_cond_domains", function(.data, ...) standardGeneric("set_cond_domains"))

#' Get Strata Weights
#'
#' @param object A handler object.
#' @return A lazy query with strata weights.
#' @export
setGeneric("get_strata_weights", function(object) standardGeneric("get_strata_weights"))

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
