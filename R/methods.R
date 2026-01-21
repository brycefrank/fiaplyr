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
setGeneric("set_tree_domains", function(.data, ...) standardGeneric("set_tree_domains"))

#' @export
setGeneric("set_cond_domains", function(.data, ...) standardGeneric("set_cond_domains"))

#' @export
setGeneric("estimate_tree", function(object, ...) standardGeneric("estimate_tree"))

#' @export
setGeneric("estimate_tree_strata", function(object, ...) standardGeneric("estimate_tree_strata"))

#' @export
setGeneric("estimate_cond_strata", function(object, ...) standardGeneric("estimate_cond_strata"))

#' @export
setGeneric("estimate_cond_eu", function(object, ...) standardGeneric("estimate_cond_eu"))

#' @export
setGeneric("estimate_cond", function(object, ...) standardGeneric("estimate_cond"))
