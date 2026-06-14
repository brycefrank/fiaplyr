---
title: "Aggregate Data"
description: "Aggregate Data"
---

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

- `schema`: An AnalysisSpec object.
- `handler`: The EvalHandler object.
- `...`: Arguments for aggregation (scoped target helper, sparse, etc.)

## Value

A lazy query with aggregates.

## Additional Details

Methods (by class)

`aggregate_data(StatusAnalysis)`: Aggregate data for Status Analysis

`aggregate_data(ChangeAnalysis)`: Aggregate data for Change Analysis
