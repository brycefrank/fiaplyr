---
title: "Scoped Helper for Ratio Estimates"
description: "Captures a numerator and denominator expression to specify a ratio estimation intent."
---

## Description

Captures a numerator and denominator expression to specify a ratio
estimation intent.

## Usage

```r
ratio(num, den)
```

## Arguments

- `num`: A scoped target helper for the numerator, e.g., `tree(VOLCFNET)`
- `den`: A scoped target helper for the denominator, e.g., `cond()`

## Value

An object of class `fiaplyr_ratio_intent`.

## Examples

```r
## Not run:

  handler |> estimate(ratio(tree(VOLCFNET), cond()))
## End(Not run)
```
