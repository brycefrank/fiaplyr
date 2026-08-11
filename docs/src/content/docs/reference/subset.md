---
title: "Subset Inventory Components of a Handler"
description: "Subsetting discards inventory components from the handler based on logical conditions. This is done in a hierarchical manner while preserving the integrity of the inventory structure. Subsetting is encouraged, as it increases the computation speed of the analysis."
---

## Description

Subsetting discards inventory components from the handler based on logical
conditions. This is done in a hierarchical manner while preserving the
integrity of the inventory structure. Subsetting is encouraged, as it
increases the computation speed of the analysis.

## Details

Subsetting is done using the [`tree()`](../tree/), [`cond()`](../cond/), and other
helpers. For example `subset(tree(STATUSCD == 1))` would retain only live
trees in later analysis. Subsetting is done hierarchically: subset statements
for [`cond()`](../cond/) apply to the conditions themselves, and trees within them,
while subset statements for [`tree()`](../tree/) apply only to trees. This ensures
that the resulting data structure remains consistent (e.g., no trees without
conditions, etc). Subsetting done higher in the hierarchy (e.g.,
[`plot()`](../plot/)) will remove all lower-level components (e.g., conditions and
trees), but retain all plots in [`aggregate()`](../aggregate/) and
[`estimate()`](../estimate/) calls to preserve the sanctity of the inventory
design.

## Usage

```r
subset(handler, ...)

## S4 method for signature 'WindowHandler'
subset(handler, ...)
```

## Arguments

- `handler`: A handler object.
- `...`: Scoped logical expressions using `tree()`, `cond()`, or `plot()` helpers.

## Value

The handler with pending filters queued.

## Additional Details

Methods (by class)

`subset(WindowHandler)`: Apply scoped filters to a WindowHandler

## Examples

```r
# Retain only live trees
handler |> subset(tree(STATUSCD == 1))
```
