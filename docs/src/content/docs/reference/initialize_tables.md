---
title: "Initialize Tables for an Analysis Spec"
description: "Initialize Tables for an Analysis Spec"
---

## Description

Initialize Tables for an Analysis Spec

## Usage

```r
initialize_tables(spec, db, evalid, backend = NULL)
```

## Arguments

- `spec`: An AnalysisSpec object.
- `db`: A DBIConnection object.
- `evalid`: The evaluation ID.
- `backend`: Optional DatabaseMapping for custom schema/table names.

## Value

A list of lazy queries.
