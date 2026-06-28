---
title: "Create a Database Mapping"
description: "A database mapping enables the use of alternative schema and table names, and is passed to the [`EvalHandler`](../evalhandler-class). When interacting with a standard FIADB, a mapping is not necessary. However, if your database uses different schema and table names, then a database mapping can be used to construct the handler. Users provide a schema name and an optional list that maps standard table names to custom table names, with the standard names as keys. `fiaplyr` only interacts with a handful of tables, see the example for the entire set."
---

## Description

A database mapping enables the use of alternative schema and table names,
and is passed to the [`EvalHandler`](../evalhandler-class). When interacting with
a standard FIADB, a mapping is not necessary. However, if your database uses
different schema and table names, then a database mapping can be used to
construct the handler. Users provide a schema name and an optional list that
maps standard table names to custom table names, with the standard names as
keys. `fiaplyr` only interacts with a handful of tables, see the example for
the entire set.

## Usage

```r
database_mapping(schema_name = NULL, table_map = list())
```

## Arguments

- `schema_name`: Optional schema name (e.g., "MY_SCHMEMA_NAME")
- `table_map`: Named list to override default table names

## Value

A [`DatabaseMapping`](../databasemapping-class) object

## Examples

```r
custom_mapping <- database_mapping(
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
