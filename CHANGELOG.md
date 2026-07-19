# Changelog

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

- Introduced `ratio(num, den, den_partitions = NULL)` as a scoped helper for ratio-estimation intent.
- Added coverage for `estimate(..., ratio(...))` dispatch and ratio-specific option handling in `EvalHandler` tests.
- Added ratio-estimator tests for denominator partition overrides.

### Changed

- Refactored `estimate` implementation, including direct ratio-intent routing from `EvalHandler`.
- Simplified `PostStratifiedRatioEstimator` to use a single handler and a ratio intent object.
- Added support for denominator-only partition overrides via `den_partitions` in ratio estimates.
- Consolidated same-scope ratio targets into one plot aggregation.
- Updated README and vignettes to use the new `estimate(..., ratio(...))` workflow.
