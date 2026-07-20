---
title: "Augment a Handler with External Data"
description: "Join external data (a local data frame or a lazy database table) onto a specific table level of a handler. This is useful for attaching reference information such as species common names, county names, or plot-level covariates. Columns added via `augment()` become available to subsequent `transform()`, `subset()`, `partition()`, and `aggregate()` calls."
---

## Description

Join external data (a local data frame or a lazy database table) onto a
specific table level of a handler. This is useful for attaching reference
information such as species common names, county names, or plot-level
covariates. Columns added via `augment()` become available to subsequent
`transform()`, `subset()`, `partition()`, and `aggregate()` calls.

## Details

The target table and join are specified using the scoped helpers
(`tree()`, `cond()`, `plot()`, `tree_history()`). The first, unnamed argument
to the helper is the data to join; named arguments configure the join:

`by`Join key(s), passed to the underlying `dplyr` join. A character
vector or a named character vector (e.g. `c("SPCD" = "code")`). If
omitted, a natural join on common columns is used.
`type`Join type: one of `"left"` (default), `"inner"`, `"right"`,
or `"full"`.
`copy`Logical controlling whether a local data frame is uploaded to
the remote database. If omitted, local data is copied automatically (with
a warning) when joined against a remote table.

## Usage

```r
augment(handler, ...)
```

## Arguments

- `handler`: A handler object.
- `...`: One or more scoped helpers describing the data to join, e.g. `tree(species_ref, by = "SPCD", type = "left")`.

## Value

The handler with pending augmentations queued.

## Examples

```r
## Not run:

species_ref <- data.frame(SPCD = c(1, 2), COMMON_NAME = c("Pine", "Oak"))
handler |>
  augment(tree(species_ref, by = "SPCD", type = "left")) |>
  partition(tree(COMMON_NAME))
## End(Not run)
```
