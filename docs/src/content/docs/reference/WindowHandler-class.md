---
title: "Class for Spatial Window"
description: "A handler that connects to a database and subsets the `PLOT` table to a spatial and temporal window. Unlike an [`EvalHandler`](../evalhandler-class/), a `WindowHandler` is not tied to an evaluation: it queries all plots within a window geometry (or bounding box), optionally restricted by state/county and inventory year."
---

## Description

A handler that connects to a database and subsets the `PLOT` table to a
spatial and temporal window. Unlike an [`EvalHandler`](../evalhandler-class/), a
`WindowHandler` is not tied to an evaluation: it queries all plots within a
window geometry (or bounding box), optionally restricted by state/county and
inventory year.

## Additional Details

Slots

- `tables`: A list of lazy queries for the tables.
- `pipeline`: Pending operations grouped by target table and operation.
- `window`: A list describing the spatial/temporal window that was applied.
