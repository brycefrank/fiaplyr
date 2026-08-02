---
title: "Select Litter Loading"
description: "Select one litter loading attribute. Supported attributes are:"
---

## Description

Select one litter loading attribute. Supported attributes are:

## Details

`DRYBIO`: dry short tons per acre
`CARBON`: short tons per acre

FIADB stores the biomass field as `LITTER_BIOMASS`.

## Usage

```r
dwm_litter(...)
```

## Arguments

- `...`: Exactly one bare attribute, optionally named to control the output column.

## Value

A structured DWM target for `[`aggregate()`](../aggregate/)` or `[`estimate()`](../estimate/)`.
