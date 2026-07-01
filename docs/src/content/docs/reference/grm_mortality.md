---
title: "Specify a mortality macro"
description: "This macro facilitates the implementation of mortality calculations in a manner consistent with the GRM paradigm, including the appropriate use of trees per acre expansion and temporal suffixes. Users supply the macro within the `tree_history` context during `aggregate` or `estimate` operations."
---

## Description

This macro facilitates the implementation of mortality calculations in a
manner consistent with the GRM paradigm, including the appropriate use of
trees per acre expansion and temporal suffixes. Users supply the macro within
the `tree_history` context during `aggregate` or `estimate` operations.

## Details

By default, the macro evaluates mortality at the midpoint of the measurement
interval expanded by the initial trees per acre. This follows the
specifications given in Bechtold and Patterson (2005). Users are able to
modify this behavior with macro arguments.

## Usage

```r
grm_mortality(
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
