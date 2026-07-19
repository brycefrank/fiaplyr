---
title: "Configure Post-Stratified Ratio Point Estimation"
description: "Creates a ratio-estimator specification for use with `estimate(handler, ratio(...), estimator = pe_post_strat_ratio())`."
---

## Description

Creates a ratio-estimator specification for use with
`estimate(handler, ratio(...), estimator = pe_post_strat_ratio())`.

## Usage

```r
pe_post_strat_ratio(var_est = "auto")
```

## Arguments

- `var_est`: A variance-estimator specification, or `"auto"`.

## Value

A `PostStratifiedRatioEstimator` specification.
