---
title: "Filter the Condition Table (Deprecated)"
description: "**Deprecated.** Use `subset(cond(...))` instead."
---

## Description

**Deprecated.** Use `subset(cond(...))` instead.

## Usage

```r
## S4 method for signature 'EvalHandler'
filter_cond(handler, ...)
```

## Arguments

- `handler`: A EvalHandler object.
- `...`: Logical predicates defined in terms of the variables in the condition table.

## Value

A EvalHandler object with pending filters.
