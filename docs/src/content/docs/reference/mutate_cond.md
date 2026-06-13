---
title: "Mutate Condition Table (Deprecated)"
description: "**Deprecated.** Use `transform(cond(...))` instead."
---

## Description

**Deprecated.** Use `transform(cond(...))` instead.

## Usage

```r
## S4 method for signature 'EvalHandler'
mutate_cond(handler, ...)
```

## Arguments

- `handler`: A EvalHandler object.
- `...`: Name-value pairs of expressions.

## Value

A EvalHandler object with pending mutations.
