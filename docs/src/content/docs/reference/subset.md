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

Subsetting is done using the `tree()` and `cond()` helpers. For
example `subset(tree(STATUSCD == 1))` would retain only live trees in later
analysis. Subsetting is done hierarchically: subset statements for `cond`
apply to the conditions themselves, and trees within them, while subset
statements for `tree` apply only to trees. This ensures that the resulting
data structure remains consistent (e.g., no trees without conditions, etc).

## Usage

```r
subset(handler, ...)
```

## Arguments

- `handler`: A handler object.
- `...`: Scoped logical expressions using `tree()`, `cond()`, or `plot()` helpers.

## Value

The handler with pending filters queued.

## Examples

```r
# Retain only live trees
handler |>
  subset(tree(STATUSCD == 1))
```
