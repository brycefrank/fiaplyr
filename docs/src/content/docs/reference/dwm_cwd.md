---
title: "Select Coarse Woody Debris"
description: "Select one coarse woody debris attribute from `COND_DWM_CALC`. Supported attributes are:"
---

## Description

Select one coarse woody debris attribute from `COND_DWM_CALC`. Supported
attributes are:

## Details

`VOLCF`: cubic feet per acre
`DRYBIO`: dry short tons per acre
`CARBON`: short tons per acre
`LPA`: pieces per acre

## Usage

```r
dwm_cwd(...)
```

## Arguments

- `...`: Exactly one bare attribute, optionally named to control the output column.

## Value

A structured DWM target for `[`aggregate()`](../aggregate/)` or `[`estimate()`](../estimate/)`.
