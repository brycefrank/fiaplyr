---
title: "Partition a Handler into Domains"
description: "Broadly, domains are unique subpopulations of inventory components (e.g., trees, etc). Domains are formed by unique combinations of domain variables, which are typically integer- or categorical-values columns in the underlying tables. This function allows the users to specify domain variables across the handler. Canonical examples include species (`SPCD`), ownership (`OWNCD`) and others."
---

## Description

Broadly, domains are unique subpopulations of inventory components (e.g.,
trees, etc). Domains are formed by unique combinations of domain variables,
which are typically integer- or categorical-values columns in the underlying
tables. This function allows the users to specify domain variables across
the handler. Canonical examples include species (`SPCD`), ownership (`OWNCD`)
and others.

## Details

Domains are specified for a table using the associated helper. For example,
`partition(tree(SPCD, STATUSCD))` would set the tree-level domains to be
unique combinations of `SPCD` and `STATUSCD`. Columns added during
[`transform()`](../transform) can be used as domain variables as well. Multiple helpers can
be mixed in a single call, such as `partition(tree(SPCD), cond(OWNCD))`.

## Usage

```r
partition(handler, ...)
```

## Arguments

- `handler`: A handler object.
- `...`: Scoped domain variable names using `tree()`, `cond()`, or `plot()` helpers.

## Value

The handler with domain variables set.

## Examples

```r
## Not run:

# Set tree-level domains to be unique combinations of species and status code
handler |>
  partition(tree(SPCD, STATUSCD))
## End(Not run)
```
