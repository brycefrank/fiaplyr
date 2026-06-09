---
title: "Get Table Reference"
description: "Get Table Reference"
---

## Description

Get Table Reference

## Usage

```r
get_table_ref(backend, standard_name)

## S4 method for signature 'DatabaseBackend'
get_table_ref(backend, standard_name)
```

## Arguments

- `backend`: A DatabaseBackend object
- `standard_name`: The standard FIA table name

## Value

A table reference (string or in_schema object)

## Additional Details

Methods (by class)

`get_table_ref(DatabaseBackend)`: Get table reference for DatabaseBackend
