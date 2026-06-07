---
title: "Explore Available Evaluations"
description: "Lists all available evaluations in the database with their descriptions."
---

Auto-generated from `man/` Rd files. Do not edit this page by hand.

## Description

Lists all available evaluations in the database with their
descriptions.

## Usage

```r
explore_evals(db, backend = NULL)
```

## Arguments

- `db`: A DBIConnection object connected to an FIA database.
- `backend`: Optional DatabaseBackend for custom schema/table names.

## Value

A tibble containing ‘EVALID’ and ‘EVAL_DESCR’.
