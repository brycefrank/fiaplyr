---
title: "Specify a reversion growth macro"
description: "Specify a reversion growth macro"
---

## Description

Specify a reversion growth macro

## Usage

```r
grm_growth_reversion(
  expr = 1,
  expander = TPA_UNADJ,
  annualize = FALSE,
  adjust = "auto",
  adjust_basis = "subptyp_grm",
  unknown_subptype = "zero"
)
```

## Arguments

- `expr`: The variable to summarize. Use `1` for stem density.
- `expander`: The expansion factor. Defaults to the macro-specific expander (e.g., `TPA_UNADJ_begin` for mortality/removal macros or `TPA_UNADJ` for ingrowth macros).
- `annualize`: Logical. If `TRUE`, divides the estimate by `REMPER`.
- `adjust`: Adjustment behavior for macro-derived targets. One of `"auto"`, `"none"`, or `"subptype"`.
- `adjust_basis`: Basis used when `adjust = "subptype"`. Currently supported: `"subptyp_grm"`.
- `unknown_subptype`: Behavior when subtype cannot be mapped to an adjustment factor. One of `"zero"`, `"drop"`, or `"warn"`.
