---
title: "Configure Post-Stratified Ratio Variance Estimation"
description: "The numerator and denominator variances are computed with the same post-stratified estimator as [`ve_post_strat()`](../ve_post_strat/). Their stratum-level covariance is $$ s_{nd,h} = \\frac{\\sum_i y_{n,hi}y_{d,hi} - (\\sum_i y_{n,hi})(\\sum_i y_{d,hi})/n_h}{n_h(n_h - 1)} $$ Covariances are rolled up through estimation units with the same post-stratification coefficient and across independent estimation units using squared population weights, giving $v_{nd}$. For $R = Y_n/Y_d$, the ratio variance uses the delta-method formula $$ v(R) = \\frac{1}{Y_d^2}\\left[ v(Y_n) + R^2v(Y_d) - 2R\\,v(Y_n,Y_d)\\right], $$ and the standard error is $\\sqrt{\\max(v(R), 0)}$."
---

## Description

The numerator and denominator variances are computed with the same
post-stratified estimator as [`ve_post_strat()`](../ve_post_strat/). Their
stratum-level covariance is

$$
s_{nd,h} = \frac{\sum_i y_{n,hi}y_{d,hi} -
(\sum_i y_{n,hi})(\sum_i y_{d,hi})/n_h}{n_h(n_h - 1)}
$$

Covariances are rolled up through estimation units with the same
post-stratification coefficient and across independent estimation units
using squared population weights, giving $v_{nd}$. For
$R = Y_n/Y_d$, the ratio variance uses the delta-method formula

$$
v(R) = \frac{1}{Y_d^2}\left[
v(Y_n) + R^2v(Y_d) - 2R\,v(Y_n,Y_d)\right],
$$

and the standard error is $\sqrt{\max(v(R), 0)}$.

## Details

This calculation also exploits sparsity. It computes only the sufficient
statistics $\sum_i y_n$, $\sum_i y_d$, and $\sum_i y_ny_d$ for observed
plot rows; rows absent from one side contribute zero to the cross-product
and do not need to be expanded into a dense plot-by-target table.

## Usage

```r
ve_post_strat_ratio()
```

## Value

A `PostStratifiedRatioVarianceEstimator` object.
