# Changelog

## Unreleased

### Changed

- Refactored GRM and DWM macro helpers into a unified `fiaplyr_target`
  system:
  - `grm_*()` and `dwm_*()` helpers now return structured target objects
    (`grm_target`, `dwm_target`) that lower to aggregation expressions via
    the `agg_expr()` generic.
  - Source files renamed (`grm-macros.R` to `grm-targets.R`, DWM macros to
    DWM targets) to reflect the new terminology.
- Renamed "macros" and "scoped helpers" to "targets" and "scopes" throughout
  the package, documentation, and vignettes:
  - `R/scoped-helpers.R` renamed to `R/scopes.R`.
  - Reference documentation index headings updated to use the new
    terminology.
- Updated vignettes (`growth_removals_mortality`, `down_woody_material`,
  `status_estimates`) to use "targets" instead of "macros".

## 0.4.0 - 2026-08-02

### Added

- Support for down woody material (DWM) analysis:
  - `dwm_analysis()` evaluation specification
  - `dwm()` scoped pipeline helper for transforming, subsetting, and
    partitioning DWM data
  - `dwm_cwd()`, `dwm_fwd()`, `dwm_pile()`, `dwm_fuel()`, `dwm_duff()`,
    and `dwm_litter()` component macros, each with documented attributes
    and units
  - Plot-level aggregation plus post-stratified point, total, margin, and
    ratio estimation for DWM targets
  - Multiple scoped targets in a single `estimate()` or `aggregate()` call,
    including named DWM targets
- A down woody material vignette demonstrating the DWM workflow
- Tests validating DWM estimates against the FIADB `fullreport` API

### Changed

- Internal restructuring of how `transform`, `subset`, `augment` and
  `partition` are implemented in handlers to allow greater extensibility

## 0.3.1 - 2026-07-25

### Highlights

- Expanded estimator documentation with post-stratified variance and ratio
  variance formulae, including the sparse sufficient-statistic approach.
- Clarified automatic selection of matching point and variance estimators.
- Improved the reference documentation site, sidebar organization, and
  documentation build tooling, including safe rendering of LaTeX formulae.

## 0.3.0 - 2026-07-19

### Added

- Added `augment()` for joining external data to handler tables.
- Added composable point and variance estimator specifications, including
  partition-aware post-stratified variance estimators.
- Added support for estimating trees-per-acre totals when tree targets are
  empty.
- Added R package CI checks and automated documentation deployment.

### Changed

- Refactored estimation to return lazy tables and support composable estimator
  dispatch.
- Streamlined ratio-estimator plot aggregation.

## 0.2.1

### Added

- Introduced `ratio(num, den, den_partitions = NULL)` as a scoped helper for
  ratio-estimation intent.
- Added coverage for `estimate(..., ratio(...))` dispatch and ratio-specific
  option handling in `EvalHandler` tests.
- Added ratio-estimator tests for denominator partition overrides.

### Changed

- Refactored `estimate` implementation, including direct ratio-intent routing
  from `EvalHandler`.
- Simplified `PostStratifiedRatioEstimator` to use a single handler and a ratio
  intent object.
- Added support for denominator-only partition overrides via `den_partitions` in
  ratio estimates.
- Consolidated same-scope ratio targets into one plot aggregation.
- Updated README and vignettes to use the new `estimate(..., ratio(...))`
  workflow.
