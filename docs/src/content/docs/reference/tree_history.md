---
title: "Scope for Tree-History-Level Expressions"
description: "Captures one or more expressions and tags them to be applied at the `tree_history` table level during lazy evaluation."
---

## Description

Captures one or more expressions and tags them to be applied at the
`tree_history` table level during lazy evaluation.

## Usage

```r
tree_history(...)
```

## Arguments

- `...`: Zero or more named or unnamed expressions.

## Value

A list of quosures tagged with `target_table = "tree_history"`.
