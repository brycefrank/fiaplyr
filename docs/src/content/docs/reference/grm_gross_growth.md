---
title: "Specify a gross growth macro"
description: "Gross growth is defined as survivor growth + mortality growth + cut growth + diversion growth + whole ingrowth volume + whole reversion volume."
---

## Description

Gross growth is defined as survivor growth + mortality growth + cut growth +
diversion growth + whole ingrowth volume + whole reversion volume.

## Usage

```r
grm_gross_growth(
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
