---
title: "Filter the Tree Table"
description: "This function applies filters to the tree table. This is more complex than a standard `dplyr::filter()` because filters are applied lazily in tandem with other pre-joined tables (e.g., `REF_SPECIES`). However, the usage and interpretation is much the same, conditional statements are provided and tree records that do not satisfy the conditions will be excluded from all subsequent operations, including aggregations and estimates."
---

## Description

This function applies filters to the tree table. This is more complex than
a standard `dplyr::filter()` because filters are applied lazily in tandem
with other pre-joined tables (e.g., `REF_SPECIES`). However, the usage and
interpretation is much the same, conditional statements are provided and
tree records that do not satisfy the conditions will be excluded from all
subsequent operations, including aggregations and estimates.

## Usage

```r
## S4 method for signature 'EvalHandler'
filter_tree(handler, ...)
```

## Arguments

- `handler`: An [`EvalHandler`](../evalhandler-class) object.
- `...`: Logical predicates defined in terms of the variables in the tree table.

## Value

An [`EvalHandler`](../evalhandler-class) object with pending filters.

## Examples

```r
handler <- eval_handler(con, evalid = 500601) |>
 filter_tree(STATUSCD == 1) # Only include live trees
```
