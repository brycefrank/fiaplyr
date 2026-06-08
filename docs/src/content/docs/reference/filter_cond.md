---
title: "Filter the Condition Table"
description: "This function applies filters to the condition table. This is more complex than a standard `dplyr::filter()` because filters are applied lazily in tandem with other pre-joined tables. For example, filtering to a specific `OWNGRPCD` will exclude all conditions *and* all trees that do not satisfy that condition, which will impact all subsequent operations."
---

## Description

This function applies filters to the condition table. This is more complex
than a standard `dplyr::filter()` because filters are applied lazily in
tandem with other pre-joined tables. For example, filtering to a specific
`OWNGRPCD` will exclude all conditions *and* all trees that do not satisfy
that condition, which will impact all subsequent operations.

## Usage

```r
## S4 method for signature 'EvalHandler'
filter_cond(handler, ...)
```

## Arguments

- `handler`: An [`EvalHandler`](../evalhandler-class) object.
- `...`: Logical predicates defined in terms of the variables in the condition table.

## Value

An [`EvalHandler`](../evalhandler-class) object with pending filters.

## Examples

```r
handler <- eval_handler(con, evalid = 500601) |>
 filter_cond(OWNGRPCD == 10)
```
