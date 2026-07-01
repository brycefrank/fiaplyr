---
title: "Create a GRM Analysis Specification"
description: "Construct a [`GRMAnalysis`](../grmanalysis-class) object for use with `[`eval_handler()`](../eval_handler)`."
---

## Description

Construct a [`GRMAnalysis`](../grmanalysis-class) object for use with
`[`eval_handler()`](../eval_handler)`.

## Usage

```r
grm_analysis(tree_basis = "all_live", land_basis = "forest_land")
```

## Arguments

- `tree_basis`: Tree basis preset. One of `all_live`, `growing_stock`, or `sawtimber`.
- `land_basis`: Land basis preset. One of `forest_land` or `timberland`.

## Value

A [`GRMAnalysis`](../grmanalysis-class) object.
