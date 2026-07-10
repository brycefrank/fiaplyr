# Changelog

## 0.2.1

### Added

- Introduced `ratio(num, den)` as a scoped helper for ratio-estimation intent.
- Added coverage for `estimate(..., ratio(...))` dispatch and ratio-specific option handling in `EvalHandler` tests.

### Changed

- Refactored `estimate` implementation, including direct ratio-intent routing from `EvalHandler`.
- Simplified `PostStratifiedRatioEstimator` to use a single handler and a ratio intent object.
- Consolidated same-scope ratio targets into one plot aggregation.
- Updated README and vignettes to use the new `estimate(..., ratio(...))` workflow.
