---
title: "Post-stratified Point Estimator"
description: "This function implements the standard FIA post-stratified point estimator. Astute statistical users will need to forgive a small abuse of nomenclature here. Indeed, this estimator is a weighted sum of post-stratified estimators across estimation units within an evaluation. We will prefer this simpler term at the risk of some confusion, as it is the standard term used in FIA documentation. Because the estimator is standard it requires the presence of `pop_stratum` and `pop_estn_unit` tables in the evaluation, which encode the post-strata weights among other details. With the default `var_est = \"auto\"`, the standard [`ve_post_strat()`](../ve_post_strat) variance estimator is selected automatically. This is equivalent to supplying `ve_post_strat()` explicitly and provides a convenient default for the standard non-ratio estimator."
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
With the default `var_est = "auto"`, the standard
[`ve_post_strat()`](../ve_post_strat) variance estimator is selected
automatically. This is equivalent to supplying `ve_post_strat()` explicitly
and provides a convenient default for the standard non-ratio estimator.

## Details

Post-stratified point estimation, in the FIA context, computes estimates for
each estimation unit using a post-stratified estimator, then sums across the
set of estimation units (assuming independence) to produce a single estimate
for the evaluation. For estimation unit $g$ we obtain

$$
\hat{Y}_g = \sum_h W_{gh} \hat{Y}_{gh}
$$

where $h$ indexes a post-stratum within estimation unit $$g$$
that unit, $\hat{Y}_{gh}$ is the stratum-specific estimate, and
$W_{gh}$ is its post-stratum weight. Then, the overall estimate is

$$
\hat{Y} = \sum_g K_g \hat{Y}_g
$$

where $K_g$ is the proportion of the total evaluation area in
estimation unit $g$. The estimator can return either the weighted
mean or the corresponding total through the `output` argument to
[`estimate()`](../estimate), wherein each estimation unit is weighted by its size
in acres, $A_g$.

## Usage

```r
pe_post_strat(var_est = "auto")
```

## Arguments

- `var_est`: A variance-estimator specification, or `"auto"`.

## Value

A `PostStratifiedEstimator` specification.
