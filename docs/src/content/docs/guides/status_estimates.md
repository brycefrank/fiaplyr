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
2.  Growing stock by county.
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

Post-stratified estimates, among others used by FIA, can be made using
the `estimate` method on the handler. By default, the `estimate` method
uses the `PostStratifiedEstimator()` class, which implements the
standard FIA post-stratified estimator used most frequently in FIA state
reports and similar products. Specify the estimation target with the
same scoped helpers used throughout the package. For example, `cond()`
estimates condition proportions, while `tree(...)` estimates tree-level
attributes.

``` r
forested_handler |>
  estimate(cond())
```

    # Source:   SQL [?? x 3]
    # Database: DuckDB 1.4.4 
      var   estimate      se
      <chr>    <dbl>   <dbl>
    1 prop     0.772 0.00909

Here, we see the proportion of forested land in Vermont is 0.772, with a
standard error of 0.009.

## Growing Stock by County

The estimation of growing stock is frequently used to assess the amount
of merchantable timber within a region. Growing stock trees are
identified in the tree table with `TREECLCD == 2`.

``` r
growing_stock_handler <- handler |>
  subset(tree(TREECLCD == 2)) |>
  partition(cond(COUNTYCD))

growing_stock_est <- growing_stock_handler |>
  estimate(tree(VOLCFGRS))

head(growing_stock_est)
```

    # Source:   SQL [?? x 4]
    # Database: DuckDB 1.4.4 
      COUNTYCD.cond var      estimate    se
              <int> <chr>       <dbl> <dbl>
    1             1 VOLCFGRS     91.1 13.3 
    2             3 VOLCFGRS    137.  13.1 
    3             5 VOLCFGRS     87.3  8.22
    4             7 VOLCFGRS     72.4 10.4 
    5             9 VOLCFGRS     88.2  8.20
    6            11 VOLCFGRS     76.7  8.58

The above results require a certain care of interpretation. Like any
domain estimate, the per-acre densities apply to the entire evaluation
area, i.e., the state of Vermont. So the estimate for COUNTYCD 1 is
interpreted as, “cubic feet of growing stock per acre in the state of
Vermont for trees that are in COUNTYCD 1” rather than “cubic feet of
growing stock per acre in COUNTYCD 1”. Hence, smaller counties will tend
to have smaller estimates, because they occupy a smaller portion of the
state, thereby deflating the per-acre density that applies to the entire
state.

Normalizing county estimates by their area is simple, but requires the
user to fetch their own data. At minimum, a data frame describing the
county proportion areas is needed. Another option is to use a ratio
estimator, which is covered in a separate vignette. However, this
imparts unnecessary variation, because the area of counties is known
with certainty, and the ratio estimator is designed to account for the
variation in the denominator (i.e., an estimate of the county size).

## Average Diameter by Species

Average diameter by species is a more complex estimate, because it
requires the use of a ratio estimator. The numerator is formed by the
sum of the diameters of trees of a given species, while the denominator
is formed by the count of trees of that species.

``` r
avg_dia_handler <- handler |>
  subset(tree(STATUSCD == 1)) |>
  partition(tree(SPCD))

avg_dia_ests <- avg_dia_handler |>
  estimate(
    ratio(tree(DIA), tree())
  ) |>
  filter(SPCD_n == SPCD_d) |>
  arrange(desc(estimate))

head(avg_dia_ests)
```

    # Source:     SQL [?? x 6]
    # Database:   DuckDB 1.4.4 
    # Ordered by: desc(estimate)
      SPCD_n SPCD_d var_n var_d      estimate    se
       <dbl>  <dbl> <chr> <chr>         <dbl> <dbl>
    1    922    922 DIA   tree_count     26.8 0    
    2    125    125 DIA   tree_count     16.5 0.473
    3    540    540 DIA   tree_count     15.8 0    
    4    742    742 DIA   tree_count     14.4 3.81 
    5    837    837 DIA   tree_count     13.3 0    
    6    832    832 DIA   tree_count     12.2 0.908

Note that the domain specifications are crossed between the numerator
and denominator (generating dia x density estimates for all pairs of
species, a behavior that is useful for other purposes), hence we filter
only to those that match. More expressive examples of ratio estimation
is given in the [ratio estimates](../ratio_estimates/) vignette.
