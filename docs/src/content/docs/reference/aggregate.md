---
title: "Aggregate Data to Plot Level"
description: "Aggregation is the process of summing tree, condition, or other component values to the plot level, which can be used to create dataframes of aggregated data, useful for a variety of applications and diagnostics."
---

## Description

Aggregation is the process of summing tree, condition, or other component
values to the plot level, which can be used to create dataframes of
aggregated data, useful for a variety of applications and diagnostics.

## Usage

```r
aggregate(handler, ...)
```

## Arguments

- `handler`: A handler object.
- `...`: A scoped target helper such as `tree(VOLCFGRS)` or `cond()`, plus any method-specific options such as `sparse = TRUE`.

## Examples

```r
## Not run:

# Aggregate gross volume to the plot level
handler |>
  aggregate(tree(VOLCFGRS))
## End(Not run)
```
