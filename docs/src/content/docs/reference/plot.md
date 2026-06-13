---
title: "Scoped Helper for Plot-Level Expressions"
description: "Captures one or more expressions and tags them to be applied at the plot table level during lazy evaluation. Used with `transform()`, `subset()`, or `partition()` to explicitly scope mutations, filters, or domain variables."
---

## Description

Captures one or more expressions and tags them to be applied at the plot
table level during lazy evaluation. Used with `transform()`, `subset()`,
or `partition()` to explicitly scope mutations, filters, or domain
variables.

## Usage

```r
plot(...)
```

## Arguments

- `...`: One or more named or unnamed expressions.

## Value

A list of quosures tagged with `target_table = "plot"`.

## Examples

```r
## Not run:

  handler |>
    subset(plot(STATECD == 50)) |>
    partition(plot(COUNTYCD))
## End(Not run)
```
