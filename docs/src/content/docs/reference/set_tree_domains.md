---
title: "Set Tree Domain Variables"
description: "Sets the domain variables used for grouping tree-level aggregations. A domain variable is a column (e.g., STATUSCD, SPCD) whose unique values or combinations define estimation domains."
---

## Description

Sets the domain variables used for grouping tree-level
aggregations. A domain variable is a column (e.g., STATUSCD, SPCD)
whose unique values or combinations define estimation domains.

## Usage

```r
## S4 method for signature 'EvalHandler'
set_tree_domains(.data, ...)
```

## Arguments

- `.data`: A EvalHandler object.
- `...`: Domain variable names (unquoted column names).

## Value

A EvalHandler object with the tree domain variables set.
