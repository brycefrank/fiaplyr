---
title: "Estimate Population Parameters"
description: "Estimate Population Parameters"
---

## Description

Estimate Population Parameters

## Usage

```r
estimate(object, ..., output = "mean", margins = FALSE)
```

## Arguments

- `object`: An estimator object.
- `...`: One or more formulas specifying estimation targets.
- `output`: Output scale, either "mean" (default) or "total".
- `margins`: Logical. If `TRUE`, returns all marginal estimates in addition to the full cross-domain estimates. Marginals are produced by re-running the estimation pipeline for every strict subset of the active domain variables, including the grand total (no domains). Dropped domain columns appear as `NA` in the output, indicating aggregation over all values of that variable. Defaults to `FALSE`.
