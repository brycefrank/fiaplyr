---
title: "Estimate Population Parameters"
description: "Estimates of population parameters are produced using the `estimate()` function, which takes a handler as the first argument, followed by a series of scoped helpers specifying the attributes of interest, e.g., `tree(VOLCFGRS)` for gross cubic-foot volume. All estimates respect the current state of the handler including transformations, subsetting, and partitions. Estimates of ratios can be produced using the `ratio()` helper, e.g., `estimate(ratio(tree(VOLCFGRS), tree(BA)))`."
---

## Description

Estimates of population parameters are produced using the `estimate()`
function, which takes a handler as the first argument, followed by a series
of scoped helpers specifying the attributes of interest, e.g.,
`tree(VOLCFGRS)` for gross cubic-foot volume. All estimates respect the
current state of the handler including transformations, subsetting, and
partitions. Estimates of ratios can be produced using the `ratio()` helper,
e.g., `estimate(ratio(tree(VOLCFGRS), tree(BA)))`.

## Details

When `estimator` is omitted, it is treated as `"auto"`. For an
`EvalHandler`, this selects the standard post-stratified estimator for
ordinary targets and the standard post-stratified ratio estimator for
`ratio()` targets. The `"missing"` method shown by
`methods("estimate")` is an internal S4 dispatch method for this omitted
argument; users do not need to specify `estimator = "missing"`.

## Usage

```r
estimate(
  object,
  ...,
  output = "mean",
  margins = FALSE,
  estimator = "auto",
  var_est = "auto"
)

## S4 method for signature 'EvalHandler,missing'
estimate(
  object,
  ...,
  output = "mean",
  margins = FALSE,
  estimator = "auto",
  var_est = "auto"
)

## S4 method for signature 'EvalHandler,PostStratifiedEstimator'
estimate(
  object,
  ...,
  output = "mean",
  margins = FALSE,
  estimator = pe_post_strat(),
  var_est = "auto"
)

## S4 method for signature 'EvalHandler,PostStratifiedRatioEstimator'
estimate(
  object,
  ...,
  output = "mean",
  margins = FALSE,
  estimator = pe_post_strat_ratio(),
  var_est = "auto"
)
```

## Arguments

- `object`: An estimator object or evaluation handler.
- `...`: Exactly one scoped target helper specifying the estimation target.
- `output`: Output scale, either "mean" (default) or "total".
- `margins`: Logical. If `TRUE`, returns all marginal estimates in addition to the full cross-domain estimates. Marginals are produced by re-running the estimation pipeline for every strict subset of the active domain variables, including the grand total (no domains). Dropped domain columns appear as `NA` in the output, indicating aggregation over all values of that variable. Defaults to `FALSE`.
- `estimator`: A point-estimator specification, or `"auto"` (default) to use standard estimator defaults based on the target helper.
- `var_est`: A variance-estimator specification, or `"auto"` (default) to use estimator-specific defaults.

## Additional Details

Functions

`estimate(object = EvalHandler, estimator = missing)`: Estimate parameters directly from an EvalHandler

`estimate(object = EvalHandler, estimator = PostStratifiedEstimator)`: Estimate parameters directly from an EvalHandler

`estimate(object = EvalHandler, estimator = PostStratifiedRatioEstimator)`: Estimate parameters directly from an EvalHandler
