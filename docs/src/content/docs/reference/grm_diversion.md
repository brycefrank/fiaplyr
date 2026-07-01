---
title: "Specify a diversion macro"
description: "Evaluates diversion variables at the measurement beginning, expanded by the initial trees per acre."
---

## Description

Evaluates diversion variables at the measurement beginning, expanded by the
initial trees per acre.

## Usage

```r
grm_diversion(
  expr = 1,
  expander = TPA_UNADJ_begin,
  annualize = FALSE,
  adjust = "auto",
  adjust_basis = "subptyp_grm",
  unknown_subptype = "zero"
)
```

## Arguments

- `expr`: The variable to summarize. Use `1` for stem density.
- `expander`: The expansion factor. Defaults to `TPA_UNADJ_begin`.
- `annualize`: Logical. If `TRUE`, divides the estimate by `REMPER`.
- `adjust`: Adjustment behavior for macro-derived targets. One of `"auto"`, `"none"`, or `"subptype"`.
- `adjust_basis`: Basis used when `adjust = "subptype"`. Currently supported: `"subptyp_grm"`.
- `unknown_subptype`: Behavior when subtype cannot be mapped to an adjustment factor. One of `"zero"`, `"drop"`, or `"warn"`.
