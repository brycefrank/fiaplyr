---
title: "Create a Downed Woody Material Analysis Specification"
description: "Construct a specification that reads condition-level downed woody material loadings from `COND_DWM_CALC`. Plot aggregation uses `_UNADJ` fields, while population estimation uses `_ADJ` fields. Fuel, duff, and litter fields are unsuffixed in FIADB and are used as stored in both modes. DWM loadings are already per-acre values and are never multiplied by tree expansion factors."
---

## Description

Construct a specification that reads condition-level downed woody material
loadings from `COND_DWM_CALC`. Plot aggregation uses `_UNADJ` fields, while
population estimation uses `_ADJ` fields. Fuel, duff, and litter fields are
unsuffixed in FIADB and are used as stored in both modes. DWM loadings are
already per-acre values and are never multiplied by tree expansion factors.

## Usage

```r
dwm_analysis()
```

## Value

A [`DWMAnalysis`](../dwmanalysis-class/) object.

## Examples

```r
handler <- eval_handler(con, 501007, spec = dwm_analysis())
handler |> aggregate(dwm_cwd(VOLCF))
handler |> estimate(dwm_fwd(CARBON, size = "ALL"))
```
