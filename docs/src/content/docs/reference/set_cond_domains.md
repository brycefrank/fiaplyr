---
title: "Set Condition Domain Variables"
description: "Sets the domain variables used for grouping condition-level aggregations. A domain variable is a column (e.g., FORTYPCD, OWNGRPCD) whose unique values or combinations define estimation domains."
---

Auto-generated from `man/` Rd files. Do not edit this page by hand.

## Description

Sets the domain variables used for grouping condition-level
aggregations. A domain variable is a column (e.g., FORTYPCD,
OWNGRPCD) whose unique values or combinations define estimation
domains.

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
