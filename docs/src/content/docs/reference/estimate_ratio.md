---
title: "Estimate Ratio"
description: "Estimate Ratio"
---

## Description

Estimate Ratio

## Usage

```r
estimate_ratio(object, ..., domain_pairing = "all", include_components = FALSE)

## S4 method for signature 'PostStratifiedRatioEstimator'
estimate_ratio(
  object,
  ...,
  domain_pairing = c("all", "matched"),
  include_components = FALSE
)
```

## Arguments

- `object`: A PostStratifiedRatioEstimator object.
- `...`: Exactly two scoped target helpers specifying the numerator and denominator targets, such as `tree(VOLCFNET)` and `cond()`.
- `domain_pairing`: Domain pairing strategy, either `"all"` (default) for all numerator/denominator domain combinations or `"matched"` to only retain rows where both sides share the same domain columns and values.
- `include_components`: Logical; if `TRUE`, append numerator and denominator component estimates and standard errors (`estimate_n`, `se_n`, `estimate_d`, `se_d`) to the output.

## Additional Details

Functions

`estimate_ratio(PostStratifiedRatioEstimator)`: Estimate ratio for PostStratifiedRatioEstimator
