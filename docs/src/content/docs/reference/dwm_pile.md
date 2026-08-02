---
title: "Select Residual Piles"
description: "Select one pile attribute. Supported attributes are:"
---

## Description

Select one pile attribute. Supported attributes are:

## Details

`VOLCF`: cubic feet per acre
`DRYBIO`: dry short tons per acre
`CARBON`: short tons per acre

## Usage

```r
dwm_pile(...)
```

## Arguments

- `...`: Exactly one bare attribute, optionally named to control the output column.

## Value

A structured DWM target for `[`aggregate()`](../aggregate/)` or `[`estimate()`](../estimate/)`.
