---
title: "Configure Post-Stratified Point Estimation"
description: "Creates an estimator specification for use with `estimate(handler, target, estimator = pe_post_strat())`."
---

## Description

Creates an estimator specification for use with
`estimate(handler, target, estimator = pe_post_strat())`.

## Usage

```r
pe_post_strat(var_est = "auto")
```

## Arguments

- `var_est`: A variance-estimator specification, or `"auto"`.

## Value

A `PostStratifiedEstimator` specification.
