---
title: "Select Coarse Woody Debris"
description: "Select one coarse woody debris attribute from `COND_DWM_CALC`. Supported attributes are `VOLCF` (cubic feet/acre), `DRYBIO` (dry short tons/acre), `CARBON` (short tons/acre), and `LPA` (pieces/acre)."
---

## Description

Select one coarse woody debris attribute from `COND_DWM_CALC`. Supported
attributes are `VOLCF` (cubic feet/acre), `DRYBIO` (dry short tons/acre),
`CARBON` (short tons/acre), and `LPA` (pieces/acre).

## Usage

```r
dwm_cwd(...)
```

## Arguments

- `...`: Exactly one bare attribute, optionally named to control the output column.

## Value

A structured DWM target for `[`aggregate()`](../aggregate/)` or `[`estimate()`](../estimate/)`.
