---
title: "Scope for Ratio Estimates"
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

- `num`: A scope for the numerator, e.g., `tree(VOLCFNET)`
- `den`: A scope for the denominator, e.g., `cond()`
- `den_partitions`: Optional denominator-only domain overrides expressed as scopes, either as a single scope (for example `cond(FORTYPCD)`) or a list of scopes (for example `list(cond(FORTYPCD), tree(SPCD))`).

## Value

An object of class `fiaplyr_ratio_intent`.

## Examples

```r
  handler |> estimate(ratio(tree(VOLCFNET), cond()))
  handler |> estimate(ratio(tree(VOLCFNET), cond(), den_partitions = list(cond(FORTYPCD))))
```
