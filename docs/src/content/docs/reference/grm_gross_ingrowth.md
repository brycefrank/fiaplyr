---
title: "Specify a gross ingrowth macro"
description: "Gross ingrowth is defined as ingrowth plus reversion ($I + R$)."
---

## Description

Gross ingrowth is defined as ingrowth plus reversion ($I + R$).

## Usage

```r
grm_gross_ingrowth(
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
