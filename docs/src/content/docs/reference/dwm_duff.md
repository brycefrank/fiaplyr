---
title: "Select Duff Loading"
description: "Select one duff loading attribute. Supported attributes are:"
---

## Description

Select one duff loading attribute. Supported attributes are:

## Details

`DRYBIO`: dry short tons per acre
`CARBON`: short tons per acre

FIADB stores the biomass field as `DUFF_BIOMASS`.

## Usage

```r
dwm_duff(...)
```

## Arguments

- `...`: Exactly one bare attribute, optionally named to control the output column.

## Value

A structured DWM target for `[`aggregate()`](../aggregate/)` or `[`estimate()`](../estimate/)`.
