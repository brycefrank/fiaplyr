#' Aggregate Data to Plot Level
#'
#' Aggregation is the process of summing tree, condition, or other component
#' values to the plot level, which can be used to create dataframes of
#' aggregated data, useful for a variety of applications and diagnostics.
#'
#' @param handler A handler object.
#' @param ... A scoped target helper such as `tree(VOLCFGRS)` or `cond()`, plus
#'   any method-specific options such as `sparse = TRUE`.
#' @export
#'
#' @examples
#' # Aggregate gross volume to the plot level
#' handler |>
#'   aggregate(tree(VOLCFGRS))
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
#' @rdname mutate_tree
#' @title Mutate Tree Table
#' @description **Deprecated.** Use `transform(tree(...))` instead.
#' @param handler A handler object.
#' @param ... Additional arguments.
setGeneric("mutate_tree", function(handler, ...) standardGeneric("mutate_tree"))

#' @export
#' @rdname mutate_cond
#' @title Mutate Condition Table
#' @description **Deprecated.** Use `transform(cond(...))` instead.
#' @param handler A handler object.
#' @param ... Additional arguments.
setGeneric("mutate_cond", function(handler, ...) standardGeneric("mutate_cond"))

#' @export
#' @rdname filter_tree
#' @title Filter Tree Table
#' @description **Deprecated.** Use `subset(tree(...))` instead.
#' @param handler A handler object.
#' @param ... Additional arguments.
setGeneric("filter_tree", function(handler, ...) standardGeneric("filter_tree"))

#' @export
#' @rdname filter_cond
#' @title Filter Condition Table
#' @description **Deprecated.** Use `subset(cond(...))` instead.
#' @param handler A handler object.
#' @param ... Additional arguments.
setGeneric("filter_cond", function(handler, ...) standardGeneric("filter_cond"))

#' @export
#' @rdname set_tree_domains
#' @title Set Tree Domain Variables
#' @description **Deprecated.** Use `partition(tree(...))` instead.
#' @param handler A handler object.
#' @param ... Additional arguments.
setGeneric("set_tree_domains", function(handler, ...) standardGeneric("set_tree_domains"))

#' @export
#' @rdname set_cond_domains
#' @title Set Condition Domain Variables
#' @description **Deprecated.** Use `partition(cond(...))` instead.
#' @param handler A handler object.
#' @param ... Additional arguments.
setGeneric("set_cond_domains", function(handler, ...) standardGeneric("set_cond_domains"))

#' Add or Modify Columns of a Handler
#'
#' Add derived columns or modify existing ones on a specific table level
#' Expressions must be wrapped in scoping helpers (`tree()`, `cond()`, etc) to
#' specify their target table.
#'
#' @param handler A handler object.
#' @param ... Scoped expressions using `tree()`, `cond()`, or `plot()` helpers.
#' @return The handler with pending mutations queued.
#' @export
#'
#' @examples
#' # Add a basal area column to the tree table
#' handler |>
#'   transform(tree(BA = 0.005454 * DIA^2))
setGeneric("transform", function(handler, ...) standardGeneric("transform"))

#' Subset Inventory Components of a Handler
#'
#' Subsetting discards inventory components from the handler based on logical
#' conditions. This is done in a hierarchical manner while preserving the
#' integrity of the inventory structure. Subsetting is encouraged, as it
#' increases the computation speed of the analysis.
#'
#' Subsetting is done using the `tree()` and `cond()` helpers. For
#' example `subset(tree(STATUSCD == 1))` would retain only live trees in later
#' analysis. Subsetting is done hierarchically: subset statements for `cond`
#' apply to the conditions themselves, and trees within them, while subset
#' statements for `tree` apply only to trees. This ensures that the resulting
#' data structure remains consistent (e.g., no trees without conditions, etc).
#'
#' @param handler A handler object.
#' @param ... Scoped logical expressions using `tree()`, `cond()`, or `plot()` helpers.
#' @return The handler with pending filters queued.
#' @export
#' @examples
#' # Retain only live trees
#' handler |>
#'  subset(tree(STATUSCD == 1))
setGeneric("subset", function(handler, ...) standardGeneric("subset"))

#' Partition a Handler into Domains
#'
#' Broadly, domains are unique subpopulations of inventory components (e.g.,
#' trees, etc). Domains are formed by unique combinations of domain variables,
#' which are typically integer- or categorical-values columns in the underlying
#' tables. This function allows the users to specify domain variables across
#' the handler. Canonical examples include species (`SPCD`), ownership (`OWNCD`)
#' and others.
#'
#' Domains are specified for a table using the associated helper. For example,
#' `partition(tree(SPCD, STATUSCD))` would set the tree-level domains to be
#' unique combinations of `SPCD` and `STATUSCD`. Columns added during
#' `transform()` can be used as domain variables as well. Multiple helpers can
#' be mixed in a single call, such as `partition(tree(SPCD), cond(OWNCD))`.
#'
#' @param handler A handler object.
#' @param ... Scoped domain variable names using `tree()`, `cond()`, or `plot()` helpers.
#' @return The handler with domain variables set.
#' @export
#'
#' @examples
#' # Set tree-level domains to be unique combinations of species and status code
#' handler |>
#'   partition(tree(SPCD, STATUSCD))
setGeneric("partition", function(handler, ...) standardGeneric("partition"))

#' Get Strata Weights
#'
#' @param handler A handler object.
#' @return A lazy query with strata weights.
#' @export
setGeneric("get_strata_weights", function(handler) standardGeneric("get_strata_weights"))

#' Estimate Population Parameters
#'
#' @param object An estimator object.
#' @param ... Exactly one scoped target helper specifying the estimation target.
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
