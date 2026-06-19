---
title: "Transform Table with Scoped Mutations"
description: "Add derived columns or modify existing ones on a specific table level (plot, condition, or tree). Expressions must be wrapped in scoping helpers (`tree()`, `cond()`, `plot()`) to specify their target table."
---

## Description

Add derived columns or modify existing ones on a specific table level
(plot, condition, or tree). Expressions must be wrapped in scoping helpers
(`tree()`, `cond()`, `plot()`) to specify their target table.

## Usage

```r
transform(handler, ...)
```

## Arguments

- `handler`: A handler object.
- `...`: Scoped expressions using `tree()`, `cond()`, or `plot()` helpers.

## Value

The handler with pending mutations queued.
