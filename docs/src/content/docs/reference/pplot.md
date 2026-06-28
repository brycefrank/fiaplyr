---
title: "Scoped Helper for Previous-Plot-Level Expressions"
description: "Captures one or more expressions and tags them to be applied at the previous plot table level during lazy evaluation."
---

## Description

Captures one or more expressions and tags them to be applied at the previous
plot table level during lazy evaluation.

## Usage

```r
pplot(...)
```

## Arguments

- `...`: Zero or more named or unnamed expressions.

## Value

A list of quosures tagged with `target_table = "pplot"`.
