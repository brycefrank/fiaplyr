---
title: "Select Residual Piles"
description: "Select one pile attribute. Supported attributes are `VOLCF` (cubic feet/acre), `DRYBIO` (dry short tons/acre), and `CARBON` (short tons/acre)."
---

## Description

Select one pile attribute. Supported attributes are `VOLCF` (cubic
feet/acre), `DRYBIO` (dry short tons/acre), and `CARBON` (short tons/acre).

## Usage

```r
dwm_pile(...)
```

## Arguments

- `...`: Exactly one bare attribute, optionally named to control the output column.

## Value

A structured DWM target for `[`aggregate()`](../aggregate/)` or `[`estimate()`](../estimate/)`.
