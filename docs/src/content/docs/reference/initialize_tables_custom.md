---
title: "Initialize Tables with Custom Schema"
description: "Initialize Tables with Custom Schema"
---

Auto-generated from `man/` Rd files. Do not edit this page by hand.

## Description

Initialize Tables with Custom Schema

## Usage

```r
initialize_tables_custom(
  schema,
  db,
  evalid,
  db_schema = NULL,
  table_names = list()
)

## S4 method for signature 'StatusAnalysis'
initialize_tables_custom(
  schema,
  db,
  evalid,
  db_schema = NULL,
  table_names = list()
)
```

## Arguments

- `schema`: An AnalysisSchema object.
- `db`: A DBIConnection object.
- `evalid`: The evaluation ID.
- `db_schema`: Optional database schema name to prefix table names.
- `table_names`: Optional named list mapping standard names to custom table names.

## Value

A list of lazy queries.

## Additional Details

Functions:

   • ‘initialize_tables_custom(StatusAnalysis)’: Initialize tables
     for Status Analysis with custom schema
