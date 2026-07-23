---
title: "Post-stratified Point Estimator"
description: "This function implements the standard FIA post-stratified point estimator. Astute statistical users will need to forgive a small abuse of nomenclature here. Indeed, this estimator is a weighted sum of post-stratified estimators across estimation units within an evaluation. We will prefer this simpler term at the risk of some confusion, as it is the standard term used in FIA documentation. Because the estimator is standard it requires the presence of `pop_stratum` and `pop_estn_unit` tables in the evaluation, which encode the post-strata weights among other details."
---

## Description

This function implements the standard FIA post-stratified point estimator.
Astute statistical users will need to forgive a small abuse of nomenclature
here. Indeed, this estimator is a weighted sum of post-stratified estimators
across estimation units within an evaluation. We will prefer this simpler
term at the risk of some confusion, as it is the standard term used in FIA
documentation. Because the estimator is standard it requires the presence
of `pop_stratum` and `pop_estn_unit` tables in the evaluation, which
encode the post-strata weights among other details.

## Details

Post-stratified point estimation computes an estimate within each stratum
and combines those estimates within an estimation unit using the stratum
weights:
$$\hat{Y}_g = \sum_h W_{gh} \hat{Y}_{gh}$$
where $g$ indexes the estimation unit, $h$ indexes a
post-stratum within that unit, $\hat{Y}_{gh}$ is the
stratum-specific estimate, and $W_{gh}$ is its population weight.
The estimator can return either the weighted mean or the corresponding total
through the
`output` argument to [`estimate()`](../estimate).

## Usage

```r
pe_post_strat(var_est = "auto")
```

## Arguments

- `var_est`: A variance-estimator specification, or `"auto"`.

## Value

A `PostStratifiedEstimator` specification.
