---
title: "Scoped Helper for Condition-Level Expressions"
description: "Captures one or more expressions and tags them to be applied at the condition table level during lazy evaluation. Used with `transform()`, `subset()`, or `partition()` to explicitly scope mutations, filters, or domain variables."
---

## Description

Captures one or more expressions and tags them to be applied at the condition
table level during lazy evaluation. Used with `transform()`, `subset()`,
or `partition()` to explicitly scope mutations, filters, or domain
variables.

## Usage

```r
cond(...)
```

## Arguments

- `...`: One or more named or unnamed expressions.

## Value

A list of quosures tagged with `target_table = "cond"`.

## Examples

```r
## Not run:

  handler |>
    subset(cond(COND_STATUS_CD == 1)) |>
    partition(cond(FORTYPCD))
## End(Not run)
```
