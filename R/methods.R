#' Retrieve Data
#'
#' Generic function to materialize data from lazy queries.
#'
#' @param object A BaseHandler object.
#' @param ... Additional arguments.
#' @export
setGeneric("fetch", function(object, ...) standardGeneric("fetch"))

#' Summarize to Plot Level
#'
#' @param object A handler object.
#' @param ... Additional arguments.
#' @export
setGeneric("summarize_plot", function(object, ...) standardGeneric("summarize_plot"))

#' Aggregate to Population Level
#'
#' @param object A handler object.
#' @param ... Additional arguments.
#' @export
setGeneric("aggregate_pop", function(object, ...) standardGeneric("aggregate_pop"))

#' @importFrom dplyr mutate
#' @export
setGeneric("mutate", function(.data, ...) standardGeneric("mutate"))

#' @importFrom dplyr group_by
#' @export
setGeneric("group_by", function(.data, ...) standardGeneric("group_by"))
