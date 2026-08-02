---
title: "Connect to an Evaluation"
description: "In FIA parlance, an evaluation specifies an area (usually a state), a time window, a set of plots, and an associated post-stratification. The `eval_handler()` function connects to an evaluation and allows users to manipulate the underlying data to produce estimates and aggregates of need. Refer to [Status Estimates](../../guides/status_estimates/) for an introduction."
---

## Description

In FIA parlance, an evaluation specifies an area (usually a state), a time
window, a set of plots, and an associated post-stratification. The
`eval_handler()` function connects to an evaluation and allows users to
manipulate the underlying data to produce estimates and aggregates of need.
Refer to [Status Estimates](../../guides/status_estimates/) for an introduction.

## Usage

```r
eval_handler(db, evalid, spec = status_analysis(), backend = NULL)
```

## Arguments

- `db`: A DBIConnection object.
- `evalid`: A numeric identifier for the evaluation.
- `spec`: An [`AnalysisSpec`](../analysisspec-class/) object. Defaults to [`status_analysis()`](../status_analysis/).
- `backend`: An optional [`database_mapping()`](../database_mapping/) for custom schema/table names.

## Value

An object of class [`EvalHandler`](../evalhandler-class/) connected to the specified evaluation.

## Examples

```r
con <- DBI::dbConnect(duckdb::duckdb(), fiadb_vt_mini_path())
handler <- eval_handler(con, evalid = 500601)
```
