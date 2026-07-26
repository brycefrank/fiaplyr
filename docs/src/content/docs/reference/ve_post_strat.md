---
title: "Configure Post-Stratified Variance Estimation"
description: "For each stratum $h$, the estimator computes the sample mean $$ \\bar{y}_h = \\frac{1}{n_h}\\sum_i y_{hi} $$ and sample variance $$ s_h^2 = \\frac{\\sum_i y_{hi}^2 - n_h\\bar{y}_h^2}{n_h(n_h - 1)}. $$ These are combined within estimation unit $g$ using the FIA post-stratification variance formula $$ v_g = \\frac{1}{n}\\sum_h\\left[w_h n_h + (1-w_h)\\frac{n_h}{n}\\right]s_h^2, $$ and then across independent estimation units as $$ v = \\sum_g w_g^2 v_g. $$ For total estimates, the final $w_g$ coefficients are replaced by estimation-unit areas. The standard error is $\\sqrt{v}$."
---

## Description

For each stratum $h$, the estimator computes the sample mean

$$
\bar{y}_h = \frac{1}{n_h}\sum_i y_{hi}
$$

and sample variance

$$
s_h^2 = \frac{\sum_i y_{hi}^2 - n_h\bar{y}_h^2}{n_h(n_h - 1)}.
$$

These are combined within estimation unit $g$ using the FIA
post-stratification variance formula

$$
v_g = \frac{1}{n}\sum_h\left[w_h n_h +
(1-w_h)\frac{n_h}{n}\right]s_h^2,
$$

and then across independent estimation units as

$$
v = \sum_g w_g^2 v_g.
$$

For total estimates, the final $w_g$ coefficients are replaced by
estimation-unit areas. The standard error is $\sqrt{v}$.

## Details

The calculation is sparse: it aggregates only observed plot rows and uses
$\sum_i y_{hi}$ and $\sum_i y_{hi}^2$ as sufficient statistics. Implicit
zero-valued rows do not need to be materialized because they contribute zero
to both sums; $n_h$ remains the full stratum sample size.

## Usage

```r
ve_post_strat()
```

## Value

A `PostStratifiedVarianceEstimator` object.
