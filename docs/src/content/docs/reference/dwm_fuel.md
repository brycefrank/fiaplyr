---
title: "Select Fuel Loading"
description: "Select `DRYBIO` (dry short tons/acre) or `CARBON` (short tons/acre). FIADB stores the biomass field as `FUEL_BIOMASS`."
---

## Description

Select `DRYBIO` (dry short tons/acre) or `CARBON` (short tons/acre). FIADB
stores the biomass field as `FUEL_BIOMASS`.

## Usage

```r
dwm_fuel(...)
```

## Arguments

- `...`: Exactly one bare attribute, optionally named to control the output column.

## Value

A structured DWM target for `[`aggregate()`](../aggregate/)` or `[`estimate()`](../estimate/)`.
