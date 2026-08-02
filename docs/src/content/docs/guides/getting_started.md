---
title: "Getting Started"
---

<!-- Generated from README.Rmd via README.md by scripts/build_guides.R. Edit README.Rmd only. -->

`fiaplyr` provides a modern, `dplyr`-inspired interface for working with
Forest Inventory and Analysis (FIA) databases. Users interact with a
`handler`, which facilitates complex database operations while
maintaining a concise syntax. Estimation and other statistical
objectives can be formed in a few lines of code.

<br clear="all">

<img src="../../readme_1.png" width="500" alt="fiaplyr workflow overview">

The verbs `subset`, `partition`, `transform`, `estimate`, and
`aggregate` are the main tools, allowing users to retain only the data
they need, form domains, create new variables, and produce estimates and
plot-level values, respectively. The interaction of these verbs creates
a flexible self-documenting framework for working with FIA data.

## Installation

You can install the development version of fiaplyr from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("brycefrank/fiaplyr")
```

## Usage

### Connecting to an FIA Database

A connection to a database is required, and the database must be
structured like FIADB or similar. `duckdb` is used below, but other
backends supported by `DBI` should work, including `SQLite` and Oracle.
We include a miniature version of the FIA database for Vermont, which
can be used for testing and learning.

``` r
library(fiaplyr) # Load fiaplyr package from local source
library(dplyr)
library(DBI)
library(duckdb)

con <- dbConnect(duckdb(), fiadb_vt_mini_path())
```

### Specifying a Handler

A `handler` is the main way users create estimates and other inventory
products from `fiaplyr`. Currently, users must specify an
`eval_handler`, which interacts with an FIA-made data structure called
an evaluation. Evaluations define an area of interest, a time period,
and an analysis context. Though evaluation-free analysis is planned for
implementation, we highly recommend them because they take advantage of
quality assurance processes used by the FIA. Users can explore the
available evaluations in the database by

``` r
explore_evals(con) |>
  head()
#> # A tibble: 3 × 2
#>   EVALID EVAL_DESCR                                                             
#>    <int> <chr>                                                                  
#> 1 500601 VERMONT 2006: 2003-2006: CURRENT AREA, CURRENT VOLUME                  
#> 2 501007 VERMONT 2010: 2006-2010: DWM                                           
#> 3 501103 VERMONT 2011: 2003-2007 to 2008-2011: AREA CHANGE, GROWTH, REMOVALS, M…
```

Because we are using the mini Vermont database, we see just two records,
but in a full FIA database there are many evaluations. For this
tutorial, we will use `500601`, which covers Vermont (`50`) for
2003-2006 (`06`), and can be used to estimate status variables (`01`).

``` r
# Initialize handler for EVALID 500601
handler <- eval_handler(con, 500601)

# Inspect the handler summary
handler
#> EvalHandler
#> ----------
#> EVALID:          500601 
#> Description:     VERMONT 2006: 2003-2006: CURRENT AREA, CURRENT VOLUME
#> 
#> Plots:           757 
#> Inventory Years: 2003 - 2006 
#> Measure Years:   2003 - 2007
```

Here, we see the evaluation number, a description, and a summary of the
sample size and time range of data collection.

### Aggregation to the Plot Level

An essential step for nearly all FIA analyses is to aggregate inventory
components, such as trees or conditions, to the plot level. This can be
done explicitly with `aggregate`. Specify a slot (e.g., `tree` or
`cond`) and one or more variables to aggregate. Here, we produce
plot-level values for `VOLCFNET` and `VOLCFGRS`

``` r
# Calculate Net Cubic Foot Volume per acre for each plot
plot_vol <- handler |>
  aggregate(tree(net_vol = VOLCFNET, VOLCFGRS))

head(plot_vol)
#> # A query:  ?? x 7
#> # Database: DuckDB 1.5.4 [bryce@Linux 7.0.0-28-generic:R 4.6.0//tmp/RtmpkTtgy0/temp_libpathafdf483de9e2/fiaplyr/fiadb_vt_mini.duckdb]
#>   PLT_CN         STATECD COUNTYCD INVYR  PLOT net_vol VOLCFGRS
#>   <chr>            <int>    <int> <int> <int>   <dbl>    <dbl>
#> 1 55955474010538      50       21  2003  1301   2159.    2850.
#> 2 73599444010538      50       21  2006  1212   3154.    3574.
#> 3 73593740010538      50       17  2006   755   1178.    1393.
#> 4 55968356010538      50       19  2004  1188   1813.    1907.
#> 5 73593053010538      50       15  2006   221   1745.    2228.
#> 6 55953855010538      50       17  2003  1478   1979.    2172.
```

Plot-level values are often used in statistical models and other
applications. However, some analyses do not explicitly need an
`aggregate` step, such as the estimation of state-wide means or totals,
so it is not always necessary to call `aggregate`. Note that columns can
be dynamically named, otherwise the stated value is used.

### Downed Woody Material

Evaluations with a `COND_DWM_CALC` table can be analyzed with
`dwm_analysis()`. Component helpers select verified DWM attributes,
while `dwm()` scopes transformations, filters, and domains to the joined
DWM table. Plot aggregation uses unadjusted columns and population
estimation uses adjusted columns; these per-acre loadings are never
multiplied by tree expansion factors.

``` r
dwm_handler <- eval_handler(con, 501007, spec = dwm_analysis()) |>
  subset(cond(COND_STATUS_CD == 1))

# Cubic feet per acre by plot, using CWD_VOLCF_UNADJ
dwm_plot_volume <- dwm_handler |>
  aggregate(dwm_cwd(cwd_volume = VOLCF))

# Short tons of carbon per acre, using all adjusted FWD size classes
dwm_carbon <- dwm_handler |>
  estimate(dwm_fwd(fwd_carbon = CARBON, size = "ALL"))

# CWD carbon per forested acre
dwm_carbon_per_forest_acre <- dwm_handler |>
  estimate(ratio(dwm_cwd(CARBON), cond()))
```

Supported components are coarse woody debris, fine woody debris, piles,
fuels, duff, and litter. `VOLCF` is returned in cubic feet per acre;
`DRYBIO` and `CARBON` are converted from FIADB pounds to short tons per
acre; and CWD `LPA` is pieces per acre. Fuel, duff, and litter
biomass/carbon columns are unsuffixed in FIADB and therefore use their
stored values for both aggregation modes.

Implicitly, `aggregate` uses a weighted sum based on trees per acre
(i.e., the `TPA_UNADJ` column), but users can specify arbitrary
functions. For example, the weighted mean of a variable can be used
instead

``` r
# Calculate Weighted Mean Volume per acre for each plot
plot_vol_wm <- handler |>
  aggregate(tree(wm_ht = sum(TPA_UNADJ * HT) / sum(TPA_UNADJ)))

head(plot_vol_wm)
#> # A query:  ?? x 6
#> # Database: DuckDB 1.5.4 [bryce@Linux 7.0.0-28-generic:R 4.6.0//tmp/RtmpkTtgy0/temp_libpathafdf483de9e2/fiaplyr/fiadb_vt_mini.duckdb]
#>   PLT_CN         STATECD COUNTYCD INVYR  PLOT wm_ht
#>   <chr>            <int>    <int> <int> <int> <dbl>
#> 1 55958357010538      50       25  2003  1320  49.2
#> 2 73604051010538      50       25  2006   895  43.5
#> 3 73607738010538      50       27  2006  1226  28.3
#> 4 73588636010538      50        9  2006  1162  22.4
#> 5 73591222010538      50       11  2006   162  24.4
#> 6 55961678010538      50        3  2004   773  28.1
```

### Transforms

Often, custom variables are needed for analysis. These can be generated
by `transform`. A canonical example is the need for basal area, a
variable that is not explicitly stored in the FIA database, but is a
simple function of diameter. Specify the slot the new variable name, and
its value.

``` r
# Calculate Basal Area per acre
ba_handler <- handler |>
  transform(tree(BA = 0.005454 * DIA^2))

plot_ba <- ba_handler |>
  aggregate(tree(BA))

# Verify the output
head(plot_ba)
#> # A query:  ?? x 6
#> # Database: DuckDB 1.5.4 [bryce@Linux 7.0.0-28-generic:R 4.6.0//tmp/RtmpkTtgy0/temp_libpathafdf483de9e2/fiaplyr/fiadb_vt_mini.duckdb]
#>   PLT_CN         STATECD COUNTYCD INVYR  PLOT    BA
#>   <chr>            <int>    <int> <int> <int> <dbl>
#> 1 73597861010538      50       21  2006  1324 183. 
#> 2 73613645010538      50        5  2006   512 132. 
#> 3 62280962010538      50       21  2005   785 140. 
#> 4 73590923010538      50       11  2006   182 104. 
#> 5 73615949010538      50        7  2006  1191  70.9
#> 6 55951984010538      50       11  2003   497  85.1
```

### Partitions

Many FIA estimates take advantage of the concept of domains, which are
subsets of trees, conditions, or other inventory components.
Partitioning is the process of assigning domains based on the values of
one or more variables. As an example, we can make species domains by
using `partition(tree(SPCD))`.

``` r
plot_ba_by_sp <- ba_handler |>
  partition(tree(SPCD)) |>
  aggregate(tree(BA)) |>
  arrange(desc(BA))

head(plot_ba_by_sp)
#> # A query:    ?? x 7
#> # Database:   DuckDB 1.5.4 [bryce@Linux 7.0.0-28-generic:R 4.6.0//tmp/RtmpkTtgy0/temp_libpathafdf483de9e2/fiaplyr/fiadb_vt_mini.duckdb]
#> # Ordered by: desc(BA)
#>   PLT_CN         STATECD COUNTYCD INVYR  PLOT  SPCD    BA
#>   <chr>            <int>    <int> <int> <int> <dbl> <dbl>
#> 1 73603051010538      50       25  2006  1140   129  242.
#> 2 55973029010538      50       25  2004   310   261  172.
#> 3 73607294010538      50       27  2006  1190   261  164.
#> 4 73599700010538      50       21  2006   761   241  164.
#> 5 62273322010538      50        3  2005   258    12  163.
#> 6 55952370010538      50       11  2003   278   371  155.
```

### Subsets

Subsets are related to partitions, but are instead used to exlclude
entire portions of the data. This is useful when analysis only involves
a subpopulation, such as a specific species, and the remaining data can
be entirely ignored. For example, if we only want to estimate basal area
for balsam fir, we can subset the handler to only include trees with
`SPCD == 12`.

``` r
plot_ba_balsam <- ba_handler |>
  subset(tree(SPCD == 12)) |>
  aggregate(tree(BA)) |>
  arrange(desc(BA))

head(plot_ba_balsam)
#> # A query:    ?? x 6
#> # Database:   DuckDB 1.5.4 [bryce@Linux 7.0.0-28-generic:R 4.6.0//tmp/RtmpkTtgy0/temp_libpathafdf483de9e2/fiaplyr/fiadb_vt_mini.duckdb]
#> # Ordered by: desc(BA)
#>   PLT_CN         STATECD COUNTYCD INVYR  PLOT    BA
#>   <chr>            <int>    <int> <int> <int> <dbl>
#> 1 62273322010538      50        3  2005   258  163.
#> 2 73610053010538      50        1  2006   325  124.
#> 3 55951098010538      50        9  2003   718  115.
#> 4 62277496010538      50       11  2005   250  107.
#> 5 73612664010538      50        3  2006  1019  107.
#> 6 73608536010538      50        1  2006   269  104.
```

As a contrast, we can use partitions to achieve a similar purpose

``` r
plot_ba_balsam <- ba_handler |>
  partition(tree(SPCD)) |>
  aggregate(tree(BA)) |>
  arrange(desc(BA)) |>
  filter(SPCD == 12) # use a standard dplyr filter to subset the aggregates

head(plot_ba_balsam)
#> # A query:    ?? x 7
#> # Database:   DuckDB 1.5.4 [bryce@Linux 7.0.0-28-generic:R 4.6.0//tmp/RtmpkTtgy0/temp_libpathafdf483de9e2/fiaplyr/fiadb_vt_mini.duckdb]
#> # Ordered by: desc(BA)
#>   PLT_CN         STATECD COUNTYCD INVYR  PLOT  SPCD    BA
#>   <chr>            <int>    <int> <int> <int> <dbl> <dbl>
#> 1 62273322010538      50        3  2005   258    12  163.
#> 2 73610053010538      50        1  2006   325    12  124.
#> 3 55951098010538      50        9  2003   718    12  115.
#> 4 62277496010538      50       11  2005   250    12  107.
#> 5 73612664010538      50        3  2006  1019    12  107.
#> 6 73608536010538      50        1  2006   269    12  104.
```

Hence, the use of subsets or partitions is a matter of user preference,
clarity, and other analytical considerations.

## Estimation

Currently, `fiaplyr` supports post-stratified estimation used in tandem
with evaluations, much in the same way as `EVALIDator`. However, the
flexibility of domain specification and mutations makes a very
expressive way to generate custom estimates. Extensions to other types
of estimators is possible, but not currently implemented.

To produce post-stratified estimates use the `estimate` method on the
handler. The `estimate` method uses the same syntax as `aggregate`, but
instead of producing plot-level values, it produces estimates of the
specified variable for the area specified by the evaluation.

The default estimator is post-stratification with Taylor variance
estimation. It can be selected explicitly when a workflow needs to make
the point- and variance-estimator choices visible:

``` r
ba_est <- ba_handler |>
  estimate(
    tree(ba = BA),
    estimator = pe_post_strat(var_est = ve_taylor())
  )
```

Recall our desire to estimate basal area, this is now straightforward.

``` r
# Create an estimator from the handler
ba_est <- ba_handler |>
  estimate(tree(ba = BA))

ba_est
#> # A query:  ?? x 3
#> # Database: DuckDB 1.5.4 [bryce@Linux 7.0.0-28-generic:R 4.6.0//tmp/RtmpkTtgy0/temp_libpathafdf483de9e2/fiaplyr/fiadb_vt_mini.duckdb]
#>   var   estimate    se
#>   <chr>    <dbl> <dbl>
#> 1 ba        97.5  1.72
```

By default, outputs are means, typically representing areal densities.
An estimate of the total can be made instead

``` r
ba_total_est <- ba_handler |>
  estimate(tree(BA), output = "total")

ba_total_est
#> # A query:  ?? x 3
#> # Database: DuckDB 1.5.4 [bryce@Linux 7.0.0-28-generic:R 4.6.0//tmp/RtmpkTtgy0/temp_libpathafdf483de9e2/fiaplyr/fiadb_vt_mini.duckdb]
#>   var     estimate        se
#>   <chr>      <dbl>     <dbl>
#> 1 BA    577424366. 10152586.
```

### Estimates of Partitions

Estimates respect partitions applied to the handler. To form basal area
estimates by species class simply do the following

``` r
# Estimate Basal Area by Species
ba_by_sp_handler <- ba_handler |>
  partition(tree(SPCD))

ba_by_sp_est <- ba_by_sp_handler |>
  estimate(tree(BA)) |>
  arrange(desc(estimate))

head(ba_by_sp_est)
#> # A query:    ?? x 4
#> # Database:   DuckDB 1.5.4 [bryce@Linux 7.0.0-28-generic:R 4.6.0//tmp/RtmpkTtgy0/temp_libpathafdf483de9e2/fiaplyr/fiadb_vt_mini.duckdb]
#> # Ordered by: desc(estimate)
#>    SPCD var   estimate    se
#>   <dbl> <chr>    <dbl> <dbl>
#> 1   318 BA       18.6  0.958
#> 2   316 BA       11.6  0.699
#> 3   261 BA        9.03 0.868
#> 4   531 BA        8.02 0.546
#> 5   129 BA        7.65 0.833
#> 6   371 BA        7.26 0.461
```

For any given domain estimate, they are interpreted as the mean value
for that domain across the state. For tree attributes, these are
per-acre densities. Hence, the first row is interpreted as: the mean
basal area per acre for `SPCD = 318` is 18.6 ft²/acre, with a standard
error of 0.96 ft²/acre across the state of Vermont. This estimate is not
normalized by forested area, which requires the more sophisticated ratio
estimator

``` r
ba_by_sp_ratio_est <- ba_by_sp_handler |>
  subset(cond(COND_STATUS_CD == 1)) |> # subset to only forested areas
  estimate(
    ratio(tree(BA), cond())
  )

ba_by_sp_ratio_est |>
  arrange(desc(estimate)) |>
  head()
#> # A query:    ?? x 5
#> # Database:   DuckDB 1.5.4 [bryce@Linux 7.0.0-28-generic:R 4.6.0//tmp/RtmpkTtgy0/temp_libpathafdf483de9e2/fiaplyr/fiadb_vt_mini.duckdb]
#> # Ordered by: desc(estimate)
#>   SPCD_n var_n var_d estimate    se
#>    <dbl> <chr> <chr>    <dbl> <dbl>
#> 1    318 BA    prop     24.1  1.01 
#> 2    316 BA    prop     15.0  0.741
#> 3    261 BA    prop     11.7  0.994
#> 4    531 BA    prop     10.4  0.596
#> 5    129 BA    prop      9.90 0.979
#> 6    371 BA    prop      9.40 0.510
```

Here, estimates are divided by the forested proportion of the state, and
the first row is interpreted as: the mean basal area per acre on
forested land for `SPCD = 318` is 24.1 ft²/acre, with a standard error
of 1.01 ft²/acre across the state of Vermont.
