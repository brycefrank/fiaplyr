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

#' Estimate Population Parameters
#'
#' @param object An estimator object.
#' @param ... Additional arguments (typically a formula).
#' @export
setGeneric("estimate", function(object, ...) standardGeneric("estimate"))
