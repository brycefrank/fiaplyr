---
title: "Scoped Helper for Downed Woody Material Expressions"
description: "Captures expressions to apply to the joined `COND_DWM_CALC` data during `[`transform()`](../transform/)`, `[`subset()`](../subset/)`, `[`partition()`](../partition/)`, or `[`augment()`](../augment/)`. Use the component-specific `dwm_*()` helpers instead when selecting an aggregation or estimation target."
---

## Description

Captures expressions to apply to the joined `COND_DWM_CALC` data during
`[`transform()`](../transform/)`, `[`subset()`](../subset/)`, `[`partition()`](../partition/)`, or `[`augment()`](../augment/)`. Use the
component-specific `dwm_*()` helpers instead when selecting an aggregation
or estimation target.

## Usage

```r
dwm(...)
```

## Arguments

- `...`: Zero or more named or unnamed expressions.

## Value

A list of quosures tagged with `target_table = "dwm"`.

## Examples

```r
handler |>
  transform(dwm(total_carbon = CWD_CARBON_ADJ + FWD_SM_CARBON_ADJ)) |>
  subset(dwm(total_carbon > 0)) |>
  partition(dwm(PHASE))
```
