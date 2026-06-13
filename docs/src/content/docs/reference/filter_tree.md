---
title: "Filter the Tree Table (Deprecated)"
description: "**Deprecated.** Use `subset(tree(...))` instead."
---

## Description

**Deprecated.** Use `subset(tree(...))` instead.

## Usage

```r
## S4 method for signature 'EvalHandler'
filter_tree(handler, ...)
```

## Arguments

- `handler`: A EvalHandler object.
- `...`: Logical predicates defined in terms of the variables in the tree table.

## Value

A EvalHandler object with pending filters.
