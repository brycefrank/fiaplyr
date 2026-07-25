---
title: "Post-Stratified Ratio Point Estimation"
description: "This function implements the FIA post-stratified ratio point estimator, a technique commonly used to estimate areal densities subset to some land basis of interest, like forested or timberland area. Much like the [`pe_post_strat()`](../pe_post_strat) estimator, this estimator computes post-stratified ratios over a set of estimation units. The estimator requires the presence of `pop_stratum` and `pop_estn_unit` tables in the handler, typically present when using [`eval_handler()`](../eval_handler)."
---

## Description

This function implements the FIA post-stratified ratio point estimator, a
technique commonly used to estimate areal densities subset to some land
basis of interest, like forested or timberland area. Much like the
[`pe_post_strat()`](../pe_post_strat) estimator, this estimator
computes post-stratified ratios over a set of estimation units. The
estimator requires the presence of `pop_stratum` and `pop_estn_unit` tables
in the handler, typically present when using [`eval_handler()`](../eval_handler).

## Details

This estimator is composed of a ratio of two [`pe_post_strat()`](../pe_post_strat)
estimators, one for the numerator and one for the denominator, yielding a
ratio estimate for the evaluation:

$$
\hat{R} = \frac{\sum_g K_g \hat{Y}_g}{\sum_g K_g \hat{X}_g}
$$

where $K_g$ is the estimation unit weight and $\hat{Y}_g$ and
$\hat{X}_g$ are two estimators of the same form, documented further in
[`pe_post_strat()`](../pe_post_strat).

## Usage

```r
pe_post_strat_ratio(var_est = "auto")
```

## Arguments

- `var_est`: A variance-estimator specification, or `"auto"`.

## Value

A `PostStratifiedRatioEstimator` specification.
