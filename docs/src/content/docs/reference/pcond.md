---
title: "Scope for Previous-Condition-Level Expressions"
description: "Captures one or more expressions and tags them to be applied at the previous condition table level during lazy evaluation."
---

## Description

Captures one or more expressions and tags them to be applied at the previous
condition table level during lazy evaluation.

## Usage

```r
pcond(...)
```

## Arguments

- `...`: Zero or more named or unnamed expressions.

## Value

A list of quosures tagged with `target_table = "pcond"`.
