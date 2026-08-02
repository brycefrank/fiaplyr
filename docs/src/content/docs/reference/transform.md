---
title: "Add or Modify Columns of a Handler"
description: "Add derived columns or modify existing ones on a specific table level Expressions must be wrapped in scoping helpers ([`tree()`](../tree/), [`cond()`](../cond/), etc) to specify their target table."
---

## Description

Add derived columns or modify existing ones on a specific table level
Expressions must be wrapped in scoping helpers ([`tree()`](../tree/), [`cond()`](../cond/), etc) to
specify their target table.

## Usage

```r
transform(handler, ...)
```

## Arguments

- `handler`: A handler object.
- `...`: Scoped expressions using `tree()`, `cond()`, or `plot()` helpers.

## Value

The handler with pending mutations queued.

## Examples

```r
# Add a basal area column to the tree table
handler |>
  transform(tree(BA = 0.005454 * DIA^2))
```
