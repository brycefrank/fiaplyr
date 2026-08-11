---
title: "Aggregate a Handler to Plot Level"
description: "Aggregates inventory data to the plot level. This is useful for creating plot-level values for statistical models and other applications. Some analyses, such as state-wide means or totals, do not require an explicit [`aggregate()`](../aggregate/) step."
---

## Description

Aggregates inventory data to the plot level. This is useful for creating
plot-level values for statistical models and other applications. Some
analyses, such as state-wide means or totals, do not require an explicit
[`aggregate()`](../aggregate/) step.

## Details

Bare variables (e.g., `tree(VOLCFGRS)`) are expanded using the per-acre
expansion factor, `TPA_UNADJ`, to produce a TPA-weighted sum per plot. This
is the standard FIA expansion. Function calls (e.g., `tree(mean(VOLCFGRS))`)
are passed to `dplyr::summarise()` using the active plot-level groupings,
allowing users to specify arbitrary aggregation functions without TPA
expansion. Functions that return a `fiaplyr_target` object, such as
[`grm_mortality()`](../grm_mortality/) and [`grm_ingrowth()`](../grm_ingrowth/), encode
their own variable and expansion logic.

## Usage

```r
aggregate(handler, ...)

## S4 method for signature 'WindowHandler'
aggregate(handler, ...)
```

## Arguments

- `handler`: A handler object.
- `...`: A scope such as `tree(VOLCFGRS)`, `tree(mean(VOLCFGRS))`, or `tree(grm_mortality(VOLCFGRS))`, plus optional arguments such as `sparse = TRUE`.

## Additional Details

Methods (by class)

`aggregate(WindowHandler)`: Aggregate a WindowHandler to the plot level

## Examples

```r
# Standard TPA expansion
handler |> aggregate(tree(VOLCFGRS))

# Custom aggregation function without TPA expansion
handler |> aggregate(tree(mean(VOLCFGRS)))

# Weighted mean using TPA_UNADJ
handler |> aggregate(tree(wm_ht = sum(TPA_UNADJ * HT) / sum(TPA_UNADJ)))
```
