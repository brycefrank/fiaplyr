---
title: "PostStratifiedEstimator"
description: "Create an object that can be used to make post-stratified estimates. The estimator can be initialized with an `EvalHandler` that defines the evaluation, or used as an unbound specification via `pe_post_strat()`."
---

## Description

Create an object that can be used to make post-stratified estimates. The
estimator can be initialized with an `EvalHandler` that defines the
evaluation, or used as an unbound specification via `pe_post_strat()`.

## Usage

```r
PostStratifiedEstimator(handler = NULL, var_est = "auto")
```

## Arguments

- `handler`: An optional EvalHandler object.
- `var_est`: A variance-estimator specification, or `"auto"` for the default non-ratio post-stratified variance estimator.
