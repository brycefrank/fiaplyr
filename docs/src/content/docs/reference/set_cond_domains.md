---
title: "Set Condition Domain Variables (Deprecated)"
description: "**Deprecated.** Use `partition(cond(...))` instead."
---

## Description

**Deprecated.** Use `partition(cond(...))` instead.

## Usage

```r
## S4 method for signature 'EvalHandler'
set_cond_domains(.data, ...)
```

## Arguments

- `.data`: A EvalHandler object.
- `...`: Domain variable names (unquoted column names).

## Value

A EvalHandler object with the condition domain variables set.
