---
title: "Create a GRM Analysis Specification"
description: "Construct a [`GRMAnalysis`](../grmanalysis-class) object for use with [`eval_handler()`](../eval_handler). GRM analysis is a specification meant to support the estimation of growth, removals, and mortality (GRM) attributes. In contrast, [`status_analysis()`](../status_analysis) is a specification meant to support the estimation of the current status of, particularly, tree- and condition-oriented attributes. Most GRM population parameters can be estimated under this specification. Generally users should seek to employ evaluations ending with `03`, indicating an evaluation engineered for GRM analysis."
---

## Description

Construct a [`GRMAnalysis`](../grmanalysis-class) object for use with
[`eval_handler()`](../eval_handler). GRM analysis is a specification meant to
support the estimation of growth, removals, and mortality (GRM) attributes.
In contrast, [`status_analysis()`](../status_analysis) is a specification meant
to support the estimation of the current status of, particularly, tree-
and condition-oriented attributes. Most GRM population parameters can be
estimated under this specification. Generally users should seek to employ
evaluations ending with `03`, indicating an evaluation engineered for GRM
analysis.

## Usage

```r
grm_analysis(tree_basis = "all_live", land_basis = "forest_land")
```

## Arguments

- `tree_basis`: Tree basis preset. One of `all_live`, `growing_stock`, or `sawtimber`.
- `land_basis`: Land basis preset. One of `forest_land` or `timberland`.

## Value

A [`GRMAnalysis`](../grmanalysis-class) object.

## Examples

```r
handler <- eval_handler(con, 501103, spec = grm_analysis())
```
