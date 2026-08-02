---
title: "Select Litter Loading"
description: "Select `DRYBIO` (dry short tons/acre) or `CARBON` (short tons/acre). FIADB stores the biomass field as `LITTER_BIOMASS`."
---

## Description

Select `DRYBIO` (dry short tons/acre) or `CARBON` (short tons/acre). FIADB
stores the biomass field as `LITTER_BIOMASS`.

## Usage

```r
dwm_litter(...)
```

## Arguments

- `...`: Exactly one bare attribute, optionally named to control the output column.

## Value

A structured DWM target for `[`aggregate()`](../aggregate/)` or `[`estimate()`](../estimate/)`.
