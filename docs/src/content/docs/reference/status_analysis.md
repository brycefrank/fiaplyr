---
title: "Create a Status Analysis Specification"
description: "Construct a [`StatusAnalysis`](../statusanalysis-class) object for use with [`eval_handler()`](../eval_handler). Status analysis is a specification meant to support the estimation of the current status of, particularly, tree- and condition-oriented attributes. In contrast, [`grm_analysis()`](../grm_analysis) is a specification meant to support the estimation of growth, removals, and mortality (GRM) attributes. Most standard population parameters can be estimated under this specification. Therefore, it is the default specification for [`eval_handler()`](../eval_handler). Generally users should seek to employ evaluations ending with `01`, indicating an evaluation engineered for status analysis, but this is not strictly enforced."
---

## Description

Construct a [`StatusAnalysis`](../statusanalysis-class) object for use with
[`eval_handler()`](../eval_handler). Status analysis is a specification meant to
support the estimation of the current status of, particularly, tree-
and condition-oriented attributes. In contrast,
[`grm_analysis()`](../grm_analysis) is a specification meant to support the
estimation of growth, removals, and mortality (GRM) attributes. Most standard
population parameters can be estimated under this specification. Therefore,
it is the default specification for [`eval_handler()`](../eval_handler).
Generally users should seek to employ evaluations ending with `01`,
indicating an evaluation engineered for status analysis, but this is not
strictly enforced.

## Details

Internally, this analysis specification builds lazy `dplyr` queries for
evaluation, estimation unit, stratum, plot, condition, and tree tables,
restricting them to the relevant selection (typically an evalid). Queries
stored within the handler are used to produce plot-level aggregates, which
are fed into estimators. Users do not typically interact with the
specification, although it plays an important role as a layer between the
handler API and the underlying database tables.

## Usage

```r
status_analysis()
```

## Value

A [`StatusAnalysis`](../statusanalysis-class) object.

## Examples

```r
handler <- eval_handler(con, 501103, spec = status_analysis())
```
