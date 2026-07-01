---
title: "Specify an accretion macro"
description: "Accretion is defined as $GS + GI + GR + GM + GC + GD$."
---

## Description

Accretion is defined as $GS + GI + GR + GM + GC + GD$.

## Usage

```r
grm_accretion(
  expr = 1,
  annualize = FALSE,
  adjust = "auto",
  adjust_basis = "subptyp_grm",
  unknown_subptype = "zero"
)
```

## Arguments

- `expr`: The variable to summarize. Use `1` for stem density.
- `annualize`: Logical. If `TRUE`, divides the estimate by `REMPER`.
- `adjust`: Adjustment behavior for macro-derived targets. One of `"auto"`, `"none"`, or `"subptype"`.
- `adjust_basis`: Basis used when `adjust = "subptype"`. Currently supported: `"subptyp_grm"`.
- `unknown_subptype`: Behavior when subtype cannot be mapped to an adjustment factor. One of `"zero"`, `"drop"`, or `"warn"`.
