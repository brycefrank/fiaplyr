---
title: "Aggregate Data"
description: "Aggregate Data"
---

Auto-generated from `man/` Rd files. Do not edit this page by hand.

## Description

Aggregate Data

## Usage

```r
aggregate_data(schema, handler, ...)

## S4 method for signature 'StatusAnalysis'
aggregate_data(schema, handler, ...)

## S4 method for signature 'ChangeAnalysis'
aggregate_data(schema, handler, ...)
```

## Arguments

- `schema`: An AnalysisSchema object.
- `handler`: The EvalHandler object.
- `...`: Arguments for aggregation (formula, sparse, etc.)

## Value

A lazy query with aggregates.

## Additional Details

Methods (by class):

   • ‘aggregate_data(StatusAnalysis)’: Aggregate data for Status
     Analysis

   • ‘aggregate_data(ChangeAnalysis)’: Aggregate data for Change
     Analysis
