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
eval_handler(db, evalid, schema = new("StatusAnalysis"), backend = NULL)
```

## Arguments

- `db`: A DBIConnection object.
- `evalid`: A numeric identifier for the evaluation.
- `schema`: An [`AnalysisSchema`](../analysisschema-class) object. Defaults to [`StatusAnalysis`](../statusanalysis-class).
- `backend`: Optional DatabaseBackend for custom schema/table names.

## Value

An object of class [`EvalHandler`](../evalhandler-class) connected to the specified evaluation.

## Examples

```r
# Connect to an evaluation with evalid 500601
con <- DBI::dbConnect(duckdb::duckdb(), fiadb_vt_mini_path())
handler <- eval_handler(con, evalid = 500601)
```
