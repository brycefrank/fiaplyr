---
title: "Status Estimates"
---

One of the most frequent needs of FIA data are estimates of status
variables. Status variables capture the state of a forest over a given
window of time, typically specified by the temporal range of an
evaluation. In this vignette, we will estimate various status variables
for the state of Vermont using an evaluation that covers the first cycle
of the inventory, 2003 to 2006. This inventory is included in a
miniature database, and readers should be able to follow along with
their own installation

We will produce estimates for three use cases:

1.  The proportion of forested land in the state.
2.  Growing stock by species group.
3.  Average diameter by species.

The first two cases are examples of post-stratified estimation, while
the third is involves the more complex ratio estimator. The ratio
estimator is covered more thouroughly in a separate vignette.

## Establishing the Handler

As always, we need to specify a handler, which manages the connection to
the database for a specific evaluation. We will use the 2003 to 2006
evaluation for Vermont status variables, which has evaluation code
`500601`.

``` r
library(fiaplyr)
library(DBI)
library(duckdb)
library(dplyr)

# Connect to the Vermont mini database
con <- dbConnect(duckdb(), fiadb_vt_mini_path())

# Create a handler for the 2003 to 2006 evaluation for Vermont status variables
handler <- eval_handler(con, 500601)

# Print the handler summary
handler
```

    EvalHandler
    ----------
    EVALID:          500601 
    Description:     VERMONT 2006: 2003-2006: CURRENT AREA, CURRENT VOLUME

    Plots:           757 
    Inventory Years: 2003 - 2006 
    Measure Years:   2003 - 2007 

## Proportion of Forested Land

The estimation of the proportion of forested land involves the use of
conditions, which are mapped portions of the plot. Further reading on
conditions can be found \[here, todo\]. If we are interested in the
estimate of the proportion of forested land, we can make a new handler
that is filtered to only include conditions that are forested, which is
obtained by filtering on `COND_STATUS_CD == 1`.

``` r
forested_handler <- handler |>
  subset(cond(COND_STATUS_CD == 1))
```

Post-stratified estimates are made using the `PostStratifiedEstimator()`
class. This class implements the standard FIA post-stratified estimator
used most frequently in FIA state reports and similar products. Pass the
handler to the estimator, and specify the estimation target with the
same scoped helpers used throughout the package. For example, `cond()`
estimates condition proportions, while `tree(...)` estimates tree-level
attributes.

``` r
ps_estimator <- PostStratifiedEstimator(forested_handler)

for_prop_est <- ps_estimator |>
  estimate(cond())

for_prop_est
```

    # A tibble: 1 × 3
      var   estimate      se
      <chr>    <dbl>   <dbl>
    1 prop     0.772 0.00909

## Growing Stock by Species Group

The estimation of growing stock is frequently used to assess the amount
of merchantable timber within a region. Growing stock trees are
identified in the tree table with `TREECLCD == 2`.

``` r
growing_stock_handler <- handler |>
  subset(tree(TREECLCD == 2)) |>
  partition(tree(SPGRPCD))

ps_estimator <- PostStratifiedEstimator(growing_stock_handler)

growing_stock_est <- ps_estimator |>
  estimate(tree(VOLCFGRS, VOLCFNET))

View(growing_stock_est)
```
