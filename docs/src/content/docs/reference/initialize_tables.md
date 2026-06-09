---
title: "Initialize Tables"
description: "Initialize Tables"
---

## Description

Initialize Tables

## Usage

```r
initialize_tables(schema, db, evalid, backend = NULL)

## S4 method for signature 'StatusAnalysis'
initialize_tables(schema, db, evalid, backend = NULL)

## S4 method for signature 'ChangeAnalysis'
initialize_tables(schema, db, evalid)
```

## Arguments

- `schema`: An AnalysisSchema object.
- `db`: A DBIConnection object.
- `evalid`: The evaluation ID.
- `backend`: Optional DatabaseBackend for custom schema/table names.

## Value

A list of lazy queries.

## Additional Details

Methods (by class)

`initialize_tables(StatusAnalysis)`: Initialize tables for Status Analysis

`initialize_tables(ChangeAnalysis)`: Initialize tables for Change Analysis (Skeleton)
