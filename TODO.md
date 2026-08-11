# Window Handler Feature Todo

Track progress for the `feature/window_handler` branch.

## 1. Coordinate retrieval interface

Create a sensible interface for coordinate retrieval, which can vary across
databases. By default, public fuzzed coordinates are in `PLOT.LON` and
`PLOT.LAT`, but we need to allow users to specify custom queries that add `LON`
and `LAT` to this table. We may be able to use `augment()` for this purpose, but
at least some error checking.

- [x] Design interface for coordinate retrieval
- [x] Default to `PLOT.LON` / `PLOT.LAT`
- [x] Allow custom queries adding `LON`/`LAT` (via `augment()`)
- [x] Error checking

Implemented: `coordinates()` S4 generic + `EvalHandler` method in
`R/coordinates.R` (exported). Supports `lon`/`lat` column overrides,
`augment(plot(...))` custom coordinates, `as_sf = TRUE` (sf in Suggests),
and validates missing/non-numeric columns. Tests in
`tests/testthat/test-coordinates.R`.

## 2. Scaffold the window_handler

Start scaffolding a `window_handler` that:

- can take sf-style geometries in a correct projection and query the database
  connection for plots within them
- takes a temporal range (defaulting to `INVYR`) that subsets the plots
- consider database-oriented spatial queries involving `STATECD` and `COUNTYCD`
  (also within the `PLOT` table)

- [x] sf-style geometry input in correct projection
- [x] Query database for plots within geometries
- [x] Temporal range subsetting (default `INVYR`)
- [x] Database-oriented spatial queries via `STATECD`/`COUNTYCD`

Implemented: `WindowHandler` S4 class + `window_handler()` constructor in
`R/WindowHandler.R` (exported). Supports `geometry` (sf/sfc, R-side
intersection in a resolved/projected CRS), `bbox` (DB-side LON/LAT range),
`invyrs`, `statecd`, single-state `countycd` shorthand, and a `county`
table of `STATECD`/`COUNTYCD` pairs. Validation rejects ambiguous
`countycd`, multi-state `countycd`, and conflicting `county`/`countycd` or
`geometry`/`bbox`. `coordinates()` gained a `WindowHandler` method via
shared internal helpers in `R/coordinates.R`. Tests in
`tests/testthat/test-WindowHandler.R`.

## 3. Estimation disabled for now

The `window_handler` should NOT allow estimation endpoints (e.g. `estimate()`) yet.
This will come in later feature releases.

- [x] Disable estimation endpoints on `window_handler`

No `estimate()` method is registered for `WindowHandler`, so calls fail with
an inherited-method error (covered by a test).
