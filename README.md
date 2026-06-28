
# fiaplyr <img src="inst/logo.png" align="right" height="200" />

<!-- badges: start -->
<!-- badges: end -->

`fiaplyr` provides a modern, `dplyr`-inspired interface for working with
Forest Inventory and Analysis (FIA) databases. With `fiaplyr`, you can
chain together operations to compute plot-level values, modify
variables, specify domains, and produce estimates. In sum, the package
is a flexible tool allowing for a broad range of analyses, but does not
enforce any guardrails like EVALIDator or other tools. Hence, some
degree of exposure to FIA data and methods is recommended before using
`fiaplyr` for analysis.

`fiaplyr` is currently in early development, and all outputs should be
treated as experimental.

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
#> # A tibble: 2 × 2
#>   EVALID EVAL_DESCR                                                             
#>    <int> <chr>                                                                  
#> 1 500601 VERMONT 2006: 2003-2006: CURRENT AREA, CURRENT VOLUME                  
#> 2 501103 VERMONT 2011: 2003-2007 to 2008-2011: AREA CHANGE, GROWTH, REMOVALS, M…
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
#> # Source:   SQL [?? x 7]
#> # Database: DuckDB 1.5.2 [bryce@Linux 6.17.0-35-generic:R 4.6.0//tmp/Rtmp7dbKq1/temp_libpath1c1e27793e0f/fiaplyr/fiadb_vt_mini.duckdb]
#>   PLT_CN         STATECD COUNTYCD INVYR  PLOT net_vol VOLCFGRS
#>   <chr>            <int>    <int> <int> <int>   <dbl>    <dbl>
#> 1 55972019010538      50       23  2004   885   997.     1277.
#> 2 62281346010538      50       21  2005   624  1851.     2256.
#> 3 55952698010538      50       11  2003  1490   677.      917.
#> 4 62282048010538      50       21  2005   911  1600.     2238.
#> 5 73596033010538      50       19  2006    49    98.7     132.
#> 6 73598663010538      50       21  2006   623  2650.     3476.
```

Plot-level values are often used in statistical models and other
applications. However, some analyses do not explicitly an `aggregate`
step, such as the estimation of state-wide means or totals, so it is not
always necessary to call `aggregate`. Note that columns can be
dynamically named, otherwise the stated value is used.

Implicitly, `aggregate` uses a weighted sum based on trees per acre
(i.e., the `TPA_UNADJ` column), but users can specify arbitrary
functions. For example, the weighted mean of a variable can be used
instead

``` r
# Calculate Weighted Mean Volume per acre for each plot
plot_vol_wm <- handler |>
  aggregate(tree(wm_ht = sum(TPA_UNADJ * HT) / sum(TPA_UNADJ)))

head(plot_vol_wm)
#> # Source:   SQL [?? x 6]
#> # Database: DuckDB 1.5.2 [bryce@Linux 6.17.0-35-generic:R 4.6.0//tmp/Rtmp7dbKq1/temp_libpath1c1e27793e0f/fiaplyr/fiadb_vt_mini.duckdb]
#>   PLT_CN         STATECD COUNTYCD INVYR  PLOT wm_ht
#>   <chr>            <int>    <int> <int> <int> <dbl>
#> 1 55955474010538      50       21  2003  1301  64.3
#> 2 73593740010538      50       17  2006   755  24.4
#> 3 73605773010538      50       27  2006   372  27.6
#> 4 73613945010538      50        5  2006  1402  17.9
#> 5 55968356010538      50       19  2004  1188  30.5
#> 6 73593053010538      50       15  2006   221  36.4
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
#> # Source:   SQL [?? x 6]
#> # Database: DuckDB 1.5.2 [bryce@Linux 6.17.0-35-generic:R 4.6.0//tmp/Rtmp7dbKq1/temp_libpath1c1e27793e0f/fiaplyr/fiadb_vt_mini.duckdb]
#>   PLT_CN         STATECD COUNTYCD INVYR  PLOT    BA
#>   <chr>            <int>    <int> <int> <int> <dbl>
#> 1 55958357010538      50       25  2003  1320  99.4
#> 2 73591222010538      50       11  2006   162  10.5
#> 3 73607738010538      50       27  2006  1226 153. 
#> 4 73588636010538      50        9  2006  1162  60.2
#> 5 73604051010538      50       25  2006   895 220. 
#> 6 55967449010538      50       17  2004   136  74.5
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
#> # Source:     SQL [?? x 7]
#> # Database:   DuckDB 1.5.2 [bryce@Linux 6.17.0-35-generic:R 4.6.0//tmp/Rtmp7dbKq1/temp_libpath1c1e27793e0f/fiaplyr/fiadb_vt_mini.duckdb]
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
#> # Source:     SQL [?? x 6]
#> # Database:   DuckDB 1.5.2 [bryce@Linux 6.17.0-35-generic:R 4.6.0//tmp/Rtmp7dbKq1/temp_libpath1c1e27793e0f/fiaplyr/fiadb_vt_mini.duckdb]
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
#> # Source:     SQL [?? x 7]
#> # Database:   DuckDB 1.5.2 [bryce@Linux 6.17.0-35-generic:R 4.6.0//tmp/Rtmp7dbKq1/temp_libpath1c1e27793e0f/fiaplyr/fiadb_vt_mini.duckdb]
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

To produce post-stratified estimates use the `PostStratifiedEstimator`
class. The `estimate` method uses the same formula syntax as
`aggregate`, but instead of producing plot-level values, it produces
estimates of the specified variable for the area specified by the
evaluation.

Recall our desire to estimate basal area, this is now straightforward.

``` r
# Create an estimator from the handler
estimator <- PostStratifiedEstimator(ba_handler)

# Estimate total Basal Area
ba_est <- estimate(estimator, tree(ba = BA))

ba_est
#> # A tibble: 1 × 3
#>   var   estimate    se
#>   <chr>    <dbl> <dbl>
#> 1 ba        97.5  1.72
```

By default, outputs are means, typically representing areal densities.
An estimate of the total can be made instead

``` r
ba_total_est <- estimate(estimator, tree(BA), output = "total")

ba_total_est
#> # A tibble: 1 × 3
#>   var     estimate        se
#>   <chr>      <dbl>     <dbl>
#> 1 BA    577424366. 10152586.
```

### Estimates of Partitions

Estimates respect partitions applied to the handler. To form basal area
estimates by species class simply do the following

``` r
# Estimate Basal Area by Species
ba_by_sp_handle <- ba_handler |>
  partition(tree(SPCD))

estimator_by_sp <- PostStratifiedEstimator(ba_by_sp_handle)

ba_by_sp_est <- estimate(estimator_by_sp, tree(BA))

head(ba_by_sp_est)
#> # A tibble: 6 × 4
#>    SPCD var   estimate      se
#>   <dbl> <chr>    <dbl>   <dbl>
#> 1   743 BA      0.428  0.133  
#> 2   935 BA      0.0794 0.0285 
#> 3   763 BA      0.0103 0.00610
#> 4   802 BA      0.173  0.0745 
#> 5    70 BA      0.0273 0.0276 
#> 6   540 BA      0.0119 0.0122
```
