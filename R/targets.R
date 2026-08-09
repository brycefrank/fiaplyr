# R/targets.R
#
# Universal target abstraction.
#
# A `fiaplyr_target` is a named, scoped request for a plot-level quantity.
# Every target carries:
#   * `scope` - the scope (target table) it is valid within, e.g.
#     "tree_history", "dwm", etc.
#   * `name` - the output column name produced when the target is aggregated
#   * target-specific fields used by the lowering method
#
# Subclasses lower themselves to a `dplyr::summarise()` expression through the
# `agg_expr()` generic, so the aggregation pipeline can treat every target
# uniformly regardless of where its source data lives.

#' Universal target constructor
#'
#' @param scope The scope (target table) the target is valid within.
#' @param name The output column name.
#' @param ... Additional target-specific fields.
#' @return A `fiaplyr_target` object.
#' @noRd
.fiaplyr_target <- function(scope, name, ...) {
  structure(
    c(list(scope = scope, name = name), list(...)),
    class = "fiaplyr_target"
  )
}

#' Lower a target to a `dplyr::summarise()` expression
#'
#' @param target A `fiaplyr_target` object.
#' @param adjusted Logical. If `TRUE`, use adjustment factors (e.g. `_ADJ`
#'   source columns or stratum subplot adjustment factors).
#' @return An unevaluated `sum(...)` expression suitable for
#'   `dplyr::summarise()`.
#' @noRd
agg_expr <- function(target, adjusted) {
  UseMethod("agg_expr")
}

#' @noRd
#' @exportS3Method agg_expr fiaplyr_target
agg_expr.fiaplyr_target <- function(target, adjusted) {
  stop(
    "`agg_expr()` is not implemented for target class `",
    class(target)[[1]],
    "`.",
    call. = FALSE
  )
}
