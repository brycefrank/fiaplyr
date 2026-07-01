---
title: "Specify a harvest removals macro"
description: "This macro facilitates the implementation of harvest-removal calculations using only cut1 and cut2 transitions."
---

## Description

This macro facilitates the implementation of harvest-removal calculations
using only cut1 and cut2 transitions.

## Usage

```r
grm_harvest_removal(
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
