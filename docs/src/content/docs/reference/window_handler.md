---
title: "Connect to a Spatial Window"
description: "Subsets the `PLOT` table to a spatial and temporal window. At least one of `geometry`, `bbox`, `statecd`, `county`, or `countycd` should be supplied to meaningfully restrict the plots; `invyrs` further restricts by inventory year. The resulting [`WindowHandler`](../windowhandler-class/) carries a lazy plot query that can be retrieved with [`coordinates()`](../coordinates/) and refined with the standard handler verbs."
---

## Description

Subsets the `PLOT` table to a spatial and temporal window. At least one of
`geometry`, `bbox`, `statecd`, `county`, or `countycd` should be supplied to
meaningfully restrict the plots; `invyrs` further restricts by inventory
year. The resulting [`WindowHandler`](../windowhandler-class/) carries a lazy plot
query that can be retrieved with [`coordinates()`](../coordinates/) and refined
with the standard handler verbs.

## Usage

```r
window_handler(
  db,
  geometry = NULL,
  crs = NULL,
  bbox = NULL,
  invyrs = NULL,
  statecd = NULL,
  countycd = NULL,
  county = NULL,
  backend = NULL
)
```

## Arguments

- `db`: A DBIConnection object.
- `geometry`: An `sf` or `sfc` geometry (point, line, or polygon) defining the window. Plots are selected via an R-side intersection of their coordinates against this geometry. Requires the `sf` package.
- `crs`: The EPSG code of `geometry` (or `bbox`). If `NULL` and `geometry` carries a coordinate reference system, it is used.
- `bbox`: A numeric vector of length 4 giving `c(xmin, ymin, xmax, ymax)` in the coordinate system of the plot coordinates (longitude/latitude for the default public fuzzed coordinates).
- `invyrs`: A numeric vector of inventory years to retain. Defaults to all years.
- `statecd`: A numeric vector of state codes. When used alone, retains all plots in those states.
- `countycd`: A numeric vector of county codes. Only valid in combination with a single-valued `statecd` (county codes are ambiguous across states).
- `county`: A data frame with `STATECD` and `COUNTYCD` columns, one row per county. Use this when selecting counties across multiple states.
- `backend`: An optional [`database_mapping()`](../database_mapping/) for custom schema/table names.

## Value

A [`WindowHandler`](../windowhandler-class/) connected to the database with
the plot query restricted to the requested window.

## Examples

```r
con <- DBI::dbConnect(duckdb::duckdb(), fiadb_vt_mini_path())

# Whole states
window_handler(con, statecd = 50)

# One state, several counties
window_handler(con, statecd = 50, countycd = c(5, 1, 3))

# Counties across states
counties <- data.frame(STATECD = c(50, 50, 9), COUNTYCD = c(5, 1, 3))
window_handler(con, county = counties)

# Spatial window
win <- sf::st_read("windham.shp") # in a projected CRS
window_handler(con, geometry = win, crs = 26918)

# With a temporal range
window_handler(con, statecd = 50, invyrs = 2008:2012)
```
