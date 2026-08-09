---
title: "Specify a diversion growth macro"
description: "Specify a diversion growth macro"
---

## Description

Specify a diversion growth macro

## Usage

```r
grm_growth_diversion(
  expr = 1,
  expander = dplyr::coalesce(TPA_UNADJ_begin, TPA_UNADJ),
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
