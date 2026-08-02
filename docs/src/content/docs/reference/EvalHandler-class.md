---
title: "Class for Evaluation Pipeline"
description: "Class for Evaluation Pipeline"
---

## Description

Class for Evaluation Pipeline

## Additional Details

Slots

- `evalid`: The evaluation ID (numeric).
- `pipeline`: Pending operations grouped by target table and operation.
- `tables`: A list of lazy queries for the tables.
- `spec`: The AnalysisSpec used.
- `internal_cache`: Environment for caching intermediate results.
