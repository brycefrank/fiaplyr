#' Retrieve Data
#'
#' Generic function to materialize data from lazy queries.
#'
#' @param object A BaseHandler object.
#' @param ... Additional arguments.
#' @export
setGeneric("fetch", function(object, ...) standardGeneric("fetch"))

#' Summarize Trees to Plot Level
#'
#' @param object A handler object.
#' @param ... Additional arguments.
#' @export
setGeneric("summarize_tree", function(object, ...) standardGeneric("summarize_tree"))

#' Summarize Conditions to Plot Level
#'
#' @param object A handler object.
#' @param ... Additional arguments.
#' @export
setGeneric("summarize_cond", function(object, ...) standardGeneric("summarize_cond"))

#' Aggregate to Population Level
#'
#' @param object A handler object.
#' @param ... Additional arguments.
#' @export
setGeneric("aggregate_pop", function(object, ...) standardGeneric("aggregate_pop"))


#' @export
setGeneric("mutate_tree", function(.data, ...) standardGeneric("mutate_tree"))

#' @export
setGeneric("mutate_cond", function(.data, ...) standardGeneric("mutate_cond"))

#' @export
setGeneric("specify_tree_domains", function(.data, ...) standardGeneric("specify_tree_domains"))

#' @export
setGeneric("specify_cond_domains", function(.data, ...) standardGeneric("specify_cond_domains"))

