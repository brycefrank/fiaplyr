---
title: "Scoped Helper for Tree-Level Expressions"
description: "Captures one or more expressions and tags them to be applied at the tree table level during lazy evaluation. Used with `transform()`, `subset()`, or `partition()` to explicitly scope mutations, filters, or domain variables."
---

## Description

Captures one or more expressions and tags them to be applied at the tree
table level during lazy evaluation. Used with `transform()`, `subset()`,
or `partition()` to explicitly scope mutations, filters, or domain
variables.

## Usage

```r
tree(...)
```

## Arguments

- `...`: One or more named or unnamed expressions.

## Value

A list of quosures tagged with `target_table = "tree"`.

## Examples

```r
## Not run:

  handler |>
    transform(tree(BA = 0.005454 * DIA^2)) |>
    subset(tree(STATUSCD == 1)) |>
    partition(tree(SPCD))
## End(Not run)
```
