---
title: "Class for Evaluation Pipeline"
description: "Class for Evaluation Pipeline"
---

## Description

Class for Evaluation Pipeline

## Additional Details

Slots

- `evalid`: The evaluation ID (numeric).
- `plot_mutations`: Pending plot-level mutation quosures.
- `plot_filters`: Pending plot-level filter quosures.
- `plot_domains`: Pending plot-level domain quosures.
- `tree_mutations`: Pending tree-level mutation quosures.
- `cond_mutations`: Pending condition-level mutation quosures.
- `tree_history_mutations`: Pending tree-history-level mutation quosures.
- `tree_domains`: Pending tree-level domain quosures.
- `cond_domains`: Pending condition-level domain quosures.
- `tree_history_domains`: Pending tree-history-level domain quosures.
- `tree_filters`: Pending tree-level filter quosures.
- `cond_filters`: Pending condition-level filter quosures.
- `tree_history_filters`: Pending tree-history-level filter quosures.
- `plot_augmentations`: Pending plot-level external-data join specs.
- `tree_augmentations`: Pending tree-level external-data join specs.
- `cond_augmentations`: Pending condition-level external-data join specs.
- `tree_history_augmentations`: Pending tree-history-level external-data join specs.
- `tables`: A list of lazy queries for the tables.
- `spec`: The AnalysisSpec used.
- `internal_cache`: Environment for caching intermediate results.
