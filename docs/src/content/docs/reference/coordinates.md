---
title: "Retrieve Plot Coordinates"
description: "Returns plot-level coordinates for an [`EvalHandler`](../evalhandler-class/). By default, the public fuzzed coordinates `LON` and `LAT` from the `PLOT` table are returned. Databases that store coordinates under different column names can pass alternate names via `lon` and `lat`. Alternatively, custom coordinate columns can be attached to the plot table with [`augment()`](../augment/) and then retrieved with `coordinates()`:"
---

## Description

Returns plot-level coordinates for an [`EvalHandler`](../evalhandler-class/). By
default, the public fuzzed coordinates `LON` and `LAT` from the `PLOT` table
are returned. Databases that store coordinates under different column names
can pass alternate names via `lon` and `lat`. Alternatively, custom
coordinate columns can be attached to the plot table with
[`augment()`](../augment/) and then retrieved with `coordinates()`:

## Details

html<div class="sourceCode">handler |>
augment(plot(coords, by = "CN")) |>
coordinates()
html</div>

Any pending plot-level [`subset()`](../subset/) filters, [`transform()`](../transform/)
mutations, and [`augment()`](../augment/) joins are applied before coordinates are
retrieved, so the result reflects the current state of the handler.

## Usage

```r
coordinates(handler, lon = "LON", lat = "LAT", as_sf = FALSE, crs = 4269, ...)

## S4 method for signature 'EvalHandler'
coordinates(handler, lon = "LON", lat = "LAT", as_sf = FALSE, crs = 4269, ...)

## S4 method for signature 'WindowHandler'
coordinates(handler, lon = "LON", lat = "LAT", as_sf = FALSE, crs = 4269, ...)
```

## Arguments

- `handler`: A handler object ([`EvalHandler`](../evalhandler-class/) or [`WindowHandler`](../windowhandler-class/)).
- `lon`: Column name containing longitude coordinates. Defaults to `"LON"`.
- `lat`: Column name containing latitude coordinates. Defaults to `"LAT"`.
- `as_sf`: Logical. If `TRUE`, returns an `sf` object with point geometry built from the coordinate columns. Defaults to `FALSE`.
- `crs`: The EPSG code of the coordinate reference system, used when `as_sf = TRUE`. Defaults to `4269` (NAD 83), the datum of the public fuzzed FIA coordinates.
- `...`: Additional arguments passed to methods.

## Value

A `tibble` with the plot identifiers `CN`,
`STATECD`, `COUNTYCD`, `INVYR`, and `PLOT` alongside the coordinate
columns. When `as_sf = TRUE`, an `sf` object with point geometry.

## Additional Details

Functions

`coordinates(EvalHandler)`: Retrieve plot coordinates for an EvalHandler

`coordinates(WindowHandler)`: Retrieve plot coordinates for a WindowHandler

## Examples

```r
con <- DBI::dbConnect(duckdb::duckdb(), fiadb_vt_mini_path())
handler <- eval_handler(con, evalid = 500601)

# Default public fuzzed coordinates
handler |> coordinates()

# As an sf object
handler |> coordinates(as_sf = TRUE)

# Custom coordinate columns via augment()
coords <- data.frame(
  CN = c(1, 2, 3),
  LON = c(-72.5, -72.6, -72.7),
  LAT = c(43.1, 43.0, 42.9)
)
handler |>
  augment(plot(coords, by = "CN")) |>
  coordinates()
```
