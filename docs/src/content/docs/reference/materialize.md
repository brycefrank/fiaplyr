---
title: "Materialize a Handler Table"
description: "Render the prepared table for a specific slot after any pending subsets, transformations, and domain settings have been applied."
---

## Description

Render the prepared table for a specific slot after any pending subsets,
transformations, and domain settings have been applied.

## Usage

```r
materialize(handler, slot)

## S4 method for signature 'EvalHandler'
materialize(handler, slot)
```

## Arguments

- `handler`: A handler object.
- `slot`: The table slot to materialize.

## Value

A lazy query for the requested table.

## Additional Details

Methods (by class)

`materialize(EvalHandler)`: Materialize a prepared table for EvalHandler
