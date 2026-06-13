---
title: "Inventory Tables and Figures"
---

The FIA routinely produces tables and figures that display series of
estimates used for reporting purposes. `fiaplyr` is not itself a tabling
or visualization engine, rather we rely on pre-existing R packages to
produce tables and figures. This vignette serves as a cookbook for
making tables with `gt` and figures with `ggplot2`. Basic products can
be made with relatively little effort. We produce a table of estimates
of growing stock by ownership and forest type, and a figure of growing
stock by county, as illustrations.

## A Crosstab of Growing Stock by Ownership and Forest Type

As always, load some prerequisite packages, establish a database
connection, and specify an `eval_handler`.

``` r
library(fiaplyr)
library(DBI)
library(duckdb)
library(dplyr)
```


    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

``` r
library(tidyr)
library(gt)
library(ggplot2)

# Connect to the Vermont mini database
con <- dbConnect(duckdb(), fiadb_vt_mini_path())

# Create a handler for the 2003 to 2006 evaluation for Vermont status variables
handler <- eval_handler(con, 500601)
```

``` r
gs_handler <- handler |>
  filter_tree(WOODLAND == 'N', TREECLCD == 2) |>
  set_cond_domains(OWNGRPCD, FORTYPCD)
```

    Warning: `filter_tree()` was deprecated in fiaplyr 0.1.0.
    ℹ Use `handler |> subset(tree(...))` instead of `handler |> filter_tree(...)`
      to apply tree-level filters.

    Warning: `set_cond_domains()` was deprecated in fiaplyr 0.1.0.
    ℹ Use `handler |> partition(cond(...))` instead of `handler |>
      set_cond_domains(...)` to set condition domain variables.

``` r
ps <- PostStratifiedEstimator(gs_handler)

gs_ests <- estimate(ps, tree(VOLCFNET), margins = TRUE)
```
