---
title: "Constructor for EvalHandler"
description: "Constructor for EvalHandler"
---

Auto-generated from `man/` Rd files. Do not edit this page by hand.

## Description

Constructor for EvalHandler

## Usage

```r
eval_handler(db, evalid, schema = new("StatusAnalysis"), backend = NULL)
```

## Arguments

- `db`: A DBIConnection object.
- `evalid`: A numeric identifier for the evaluation.
- `schema`: An AnalysisSchema object. Defaults to StatusAnalysis.
- `backend`: Optional DatabaseBackend for custom schema/table names.
