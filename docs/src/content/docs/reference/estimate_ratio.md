---
title: "Estimate Ratio"
description: "Estimate Ratio"
---

## Description

Estimate Ratio

## Usage

```r
estimate_ratio(object, ..., domain_pairing = "all")

## S4 method for signature 'PostStratifiedRatioEstimator'
estimate_ratio(object, ..., domain_pairing = c("all", "matched"))
```

## Arguments

- `object`: A PostStratifiedRatioEstimator object.
- `...`: Ratio formulas.
- `domain_pairing`: Domain pairing strategy, either ‘"all"’ (default) for all numerator/denominator domain combinations or ‘"matched"’ to only retain rows where both sides share the same domain columns and values.

## Additional Details

Functions:

   • ‘estimate_ratio(PostStratifiedRatioEstimator)’: Estimate
     ratio for PostStratifiedRatioEstimator
