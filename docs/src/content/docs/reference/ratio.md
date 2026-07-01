---
title: "Scoped Helper for Ratio Estimates"
description: "Captures a numerator and denominator expression to specify a ratio estimation intent."
---

## Description

Captures a numerator and denominator expression to specify a ratio
estimation intent.

## Usage

```r
ratio(num, den, den_partitions = NULL)
```

## Arguments

- `num`: A scoped target helper for the numerator, e.g., `tree(VOLCFNET)`
- `den`: A scoped target helper for the denominator, e.g., `cond()`
- `den_partitions`: Optional denominator-only domain overrides expressed as scoped helpers, either as a single helper (for example `cond(FORTYPCD)`) or a list of helpers (for example `list(cond(FORTYPCD), tree(SPCD))`).

## Value

An object of class `fiaplyr_ratio_intent`.

## Examples

```r
## Not run:

  handler |> estimate(ratio(tree(VOLCFNET), cond()))
  handler |> estimate(ratio(tree(VOLCFNET), cond(), den_partitions = list(cond(FORTYPCD))))
## End(Not run)
```
