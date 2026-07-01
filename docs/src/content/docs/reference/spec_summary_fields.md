---
title: "Build Spec-Specific Summary Fields"
description: "Build Spec-Specific Summary Fields"
---

## Description

Build Spec-Specific Summary Fields

## Usage

```r
spec_summary_fields(spec, handler)

## S4 method for signature 'AnalysisSpec'
spec_summary_fields(spec, handler)

## S4 method for signature 'StatusAnalysis'
spec_summary_fields(spec, handler)

## S4 method for signature 'GRMAnalysis'
spec_summary_fields(spec, handler)
```

## Arguments

- `spec`: An AnalysisSpec object.
- `handler`: The EvalHandler object.

## Value

A named list of summary fields.

## Additional Details

Functions

`spec_summary_fields(AnalysisSpec)`: Default summary fields for AnalysisSpec

`spec_summary_fields(StatusAnalysis)`: StatusAnalysis-specific summary fields

`spec_summary_fields(GRMAnalysis)`: GRMAnalysis-specific summary fields
