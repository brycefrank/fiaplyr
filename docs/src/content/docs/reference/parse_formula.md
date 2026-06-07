---
title: "Parse Estimation Formula"
description: "Parses a formula of the form ‘slot ~ variable | variable2’ into its components."
---

## Description

Parses a formula of the form ‘slot ~ variable | variable2’ into
its components.

## Usage

```r
parse_formula(f)
```

## Arguments

- `f`: A formula.

## Value

A list containing the slot name and a character vector of target
variables.

## Examples

```r
parse_formula(tree ~ VOLCFGRS | VOLCFNET)
parse_formula(cond ~ 1)
```
