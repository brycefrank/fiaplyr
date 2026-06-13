---
title: "Create a Database Backend"
description: "A database backend enables the use of alternative schema and table names, and is passed to the [`EvalHandler`](../evalhandler-class). When interacting with a standard FIADB, a backend is not necessary. However, if your database uses different schema and table names, then a database backend can be used to construct the handler. Users provide a schema name and an optional list that maps standard table names to custom table names, with the standard names as keys. `fiaplyr` only interacts with a handful of tables, see the example for the entire set."
---

## Description

A database backend enables the use of alternative schema and table names,
and is passed to the [`EvalHandler`](../evalhandler-class). When interacting with
a standard FIADB, a backend is not necessary. However, if your database uses
different schema and table names, then a database backend can be used to
construct the handler. Users provide a schema name and an optional list that
maps standard table names to custom table names, with the standard names as
keys. `fiaplyr` only interacts with a handful of tables, see the example for
the entire set.

## Usage

```r
database_backend(schema_name = NULL, table_map = list())
```

## Arguments

- `schema_name`: Optional schema name (e.g., "MY_SCHMEMA_NAME")
- `table_map`: Named list to override default table names

## Value

A [`DatabaseBackend`](../databasebackend-class) object

## Examples

```r
custom_backend <- database_backend(
  schema_name = "MY_SCHEMA",
  table_map = list(
    POP_EVAL = "MY_POP_EVAL",
    POP_ESTN_UNIT = "MY_POP_ESTN_UNIT",
    POP_STRATUM = "MY_POP_STRATUM",
    POP_PLOT_STRATUM_ASSGN = "MY_POP_PLOT_STRAT",
    PLOT = "MY_PLOT",
    COND = "MY_COND",
    TREE = "MY_TREE",
    REF_SPECIES = "MY_REF_SPECIES",
    SUBP_COND = "MY_SUBP_COND"
  )
)
```
