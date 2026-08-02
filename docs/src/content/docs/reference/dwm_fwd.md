---
title: "Select Fine Woody Debris"
description: "Select one fine woody debris attribute. Supported attributes are `VOLCF` (cubic feet/acre), `DRYBIO` (dry short tons/acre), and `CARBON` (short tons/acre)."
---

## Description

Select one fine woody debris attribute. Supported attributes are `VOLCF`
(cubic feet/acre), `DRYBIO` (dry short tons/acre), and `CARBON` (short
tons/acre).

## Usage

```r
dwm_fwd(..., size = NULL)
```

## Arguments

- `...`: Exactly one bare attribute, optionally named to control the output column.
- `size`: Fine woody debris size class: `"SM"`, `"MD"`, `"LG"`, or `"ALL"` to sum all three classes.

## Value

A structured DWM target for `[`aggregate()`](../aggregate/)` or `[`estimate()`](../estimate/)`.
