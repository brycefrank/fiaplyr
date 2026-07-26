#' Aggregate a Handler to Plot Level
#'
#' Aggregates inventory data to the plot level. This is useful for creating
#' plot-level values for statistical models and other applications. Some
#' analyses, such as state-wide means or totals, do not require an explicit
#' [aggregate()][aggregate] step.
#'
#' Bare variables (e.g., `tree(VOLCFGRS)`) are expanded using the per-acre
#' expansion factor, `TPA_UNADJ`, to produce a TPA-weighted sum per plot. This
#' is the standard FIA expansion. Function calls (e.g., `tree(mean(VOLCFGRS))`)
#' are passed to `dplyr::summarise()` using the active plot-level groupings,
#' allowing users to specify arbitrary aggregation functions without TPA
#' expansion. Functions that return a `fiaplyr_macro` object, such as
#' [grm_mortality()][grm_mortality] and [grm_ingrowth()][grm_ingrowth], encode
#' their own variable and expansion logic.
#'
#' @param handler A handler object.
#' @param ... A scoped target helper such as `tree(VOLCFGRS)`,
#'   `tree(mean(VOLCFGRS))`, or `tree(grm_mortality(VOLCFGRS))`, plus optional
#'   arguments such as `sparse = TRUE`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Standard TPA expansion
#' handler |> aggregate(tree(VOLCFGRS))
#'
#' # Custom aggregation function without TPA expansion
#' handler |> aggregate(tree(mean(VOLCFGRS)))
#'
#' # Weighted mean using TPA_UNADJ
#' handler |> aggregate(tree(wm_ht = sum(TPA_UNADJ * HT) / sum(TPA_UNADJ)))
#' }
setGeneric("aggregate", function(handler, ...) standardGeneric("aggregate"))

#' Add or Modify Columns of a Handler
#'
#' Add derived columns or modify existing ones on a specific table level
#' Expressions must be wrapped in scoping helpers ([tree()][tree], [cond()][cond], etc) to
#' specify their target table.
#'
#' @param handler A handler object.
#' @param ... Scoped expressions using `tree()`, `cond()`, or `plot()` helpers.
#' @return The handler with pending mutations queued.
#' @export
#'
#' @examples
#' \dontrun{
#' # Add a basal area column to the tree table
#' handler |>
#'   transform(tree(BA = 0.005454 * DIA^2))
#' }
setGeneric("transform", function(handler, ...) standardGeneric("transform"))

#' Subset Inventory Components of a Handler
#'
#' Subsetting discards inventory components from the handler based on logical
#' conditions. This is done in a hierarchical manner while preserving the
#' integrity of the inventory structure. Subsetting is encouraged, as it
#' increases the computation speed of the analysis.
#'
#' Subsetting is done using the [tree()][tree], [cond()][cond], and other
#' helpers. For example `subset(tree(STATUSCD == 1))` would retain only live
#' trees in later analysis. Subsetting is done hierarchically: subset statements
#' for [cond()][cond] apply to the conditions themselves, and trees within them,
#' while subset statements for [tree()][tree] apply only to trees. This ensures
#' that the resulting data structure remains consistent (e.g., no trees without
#' conditions, etc). Subsetting done higher in the hierarchy (e.g.,
#' [plot()][plot]) will remove all lower-level components (e.g., conditions and
#' trees), but retain all plots in [aggregate()][aggregate] and
#' [estimate()][estimate] calls to preserve the sanctity of the inventory
#' design.
#'
#' @param handler A handler object.
#' @param ... Scoped logical expressions using `tree()`, `cond()`, or `plot()`
#' helpers.
#'
#' @return The handler with pending filters queued.
#' @export
#' @examples \dontrun{
#' # Retain only live trees
#' handler |> subset(tree(STATUSCD == 1))
#' }
setGeneric("subset", function(handler, ...) standardGeneric("subset"))

#' Partition a Handler into Domains
#'
#' Broadly, domains are unique subpopulations of inventory components (e.g.,
#' trees, etc). Domains are formed by unique combinations of domain variables,
#' which are typically integer- or categorical-valued columns in the underlying
#' tables. This function allows the users to specify domain variables across
#' the handler. Canonical examples include species (`SPCD`), ownership (`OWNCD`)
#' and others.
#'
#' Domains are specified for a table using the associated helper. For example,
#' `partition(tree(SPCD, STATUSCD))` would set the tree-level domains to be
#' unique combinations of `SPCD` and `STATUSCD`. Columns added during
#' [transform()][transform] can be used as domain variables as well. Multiple helpers can
#' be mixed in a single call, such as `partition(tree(SPCD), cond(OWNCD))`.
#'
#' @param handler A handler object.
#' @param ... Scoped domain variable names using `tree()`, `cond()`, or `plot()` helpers.
#' @return The handler with domain variables set.
#' @export
#'
#' @examples
#' \dontrun{
#' # Set tree-level domains to be unique combinations of species and status code
#' handler |>
#'   partition(tree(SPCD, STATUSCD))
#' }
setGeneric("partition", function(handler, ...) standardGeneric("partition"))

#' Augment a Handler with External Data
#'
#' Join external data (a local data frame or a lazy database table) onto a
#' specific table level of a handler. This is useful for attaching reference
#' information such as species common names, county names, or plot-level
#' covariates. Columns added via `augment()` become available to subsequent
#' [transform()][transform], [subset()][subset], [partition()][partition], and
#' [aggregate()][aggregate] calls.
#'
#' The target table and join are specified using the scoped helpers
#' (e.g., [tree()][tree], [cond()][cond], etc.) The first, unnamed argument
#' to the helper is the data to join; named arguments configure the join:
#'
#' \describe{
#'   \item{`by`}{Join key(s), passed to the underlying `dplyr` join. A character
#'     vector or a named character vector (e.g. `c("SPCD" = "code")`). If
#'     omitted, a natural join on common columns is used.}
#'   \item{`type`}{Join type: one of `"left"` (default), `"inner"`, `"right"`,
#'     or `"full"`.}
#'   \item{`copy`}{Logical controlling whether a local data frame is uploaded to
#'     the remote database. If omitted, local data is copied automatically (with
#'     a warning) when joined against a remote table.}
#' }
#'
#' @param handler A handler object.
#' @param ... One or more scoped helpers describing the data to join, e.g.
#'   `tree(species_ref, by = "SPCD", type = "left")`.
#' @return The handler with pending augmentations queued.
#' @export
#'
#' @examples
#' \dontrun{
#' species_ref <- data.frame(SPCD = c(1, 2), COMMON_NAME = c("Pine", "Oak"))
#' handler |>
#'   augment(tree(species_ref, by = "SPCD", type = "left")) |>
#'   partition(tree(COMMON_NAME))
#' }
setGeneric("augment", function(handler, ...) standardGeneric("augment"))

#' Materialize a Handler Table
#'
#' Render the prepared table for a specific slot after any pending subsets,
#' transformations, and domain settings have been applied.
#'
#' @param handler A handler object.
#' @param slot The table slot to materialize.
#' @return A lazy query for the requested table.
#' @export
setGeneric("materialize", function(handler, slot) {
  standardGeneric("materialize")
})

#' Get Strata Weights
#'
#' @param handler A handler object.
#' @return A lazy query with strata weights.
#' @noRd
setGeneric("get_strata_weights", function(handler) {
  standardGeneric("get_strata_weights")
})

#' Estimate Population Parameters
#'
#' Estimates of population parameters are produced using the `estimate()`
#' function, which takes a handler as the first argument, followed by a series
#' of scoped helpers specifying the attributes of interest, e.g.,
#' `tree(VOLCFGRS)` for gross cubic-foot volume. All estimates respect the
#' current state of the handler including transformations, subsetting, and
#' partitions. Estimates of ratios can be produced using the `ratio()` helper,
#' e.g., `estimate(ratio(tree(VOLCFGRS), tree(BA)))`.
#'
#' @param object An estimator object or evaluation handler.
#' @param ... Exactly one scoped target helper specifying the estimation target.
#' @param output Output scale, either "mean" (default) or "total".
#' @param margins Logical. If `TRUE`, returns all marginal estimates in addition
#'   to the full cross-domain estimates. Marginals are produced by re-running
#'   the estimation pipeline for every strict subset of the active domain
#'   variables, including the grand total (no domains). Dropped domain columns
#'   appear as `NA` in the output, indicating aggregation over all values of
#'   that variable. Defaults to `FALSE`.
#' @param estimator A point-estimator specification, or `"auto"` (default) to
#'   use standard estimator defaults based on the target helper.
#' @param var_est A variance-estimator specification, or `"auto"` (default) to
#'   use estimator-specific defaults.
#' @details
#' When `estimator` is omitted, it is treated as `"auto"`. For an
#' `EvalHandler`, this selects the standard post-stratified estimator for
#' ordinary targets and the standard post-stratified ratio estimator for
#' `ratio()` targets. The `"missing"` method shown by
#' `methods("estimate")` is an internal S4 dispatch method for this omitted
#' argument; users do not need to specify `estimator = "missing"`.
#' @export
setGeneric(
  "estimate",
  function(
    object,
    ...,
    output = "mean",
    margins = FALSE,
    estimator = "auto",
    var_est = "auto"
  ) {
    standardGeneric("estimate")
  },
  signature = c("object", "estimator")
)
