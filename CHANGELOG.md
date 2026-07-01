# Changelog

## 0.2.1

### Added

- Introduced `ratio(num, den, den_partitions = NULL)` as a scoped helper for ratio-estimation intent.
- Added coverage for `estimate(..., ratio(...))` dispatch and ratio-specific option handling in `EvalHandler` tests.
- Added ratio-estimator tests for denominator partition overrides.

### Changed

- Refactored `estimate` implementation, including direct ratio-intent routing from `EvalHandler`.
- Simplified `PostStratifiedRatioEstimator` to use a single handler and a ratio intent object.
- Added support for denominator-only partition overrides via `den_partitions` in ratio estimates.
- Updated README and vignettes to use the new `estimate(..., ratio(...))` workflow.
