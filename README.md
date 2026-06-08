
# fiaplyr <img src="inst/logo.png" align="right" height="200" />

<!-- badges: start -->
<!-- badges: end -->

`fiaplyr` provides a modern, `dplyr`-style interface for working with
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
devtools::load_all() # Load fiaplyr package from local source
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
#> # A tibble: 1 × 2
#>   EVALID EVAL_DESCR                                           
#>    <int> <chr>                                                
#> 1 500601 VERMONT 2006: 2003-2006: CURRENT AREA, CURRENT VOLUME
```

Because we are using the mini Vermont database, we see just one record,
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
plot-level values for `VOLCFNET` and `VOLCFGRS`, separating with a `|`
as we go.

``` r
# Calculate Net Cubic Foot Volume per acre for each plot
# The formula syntax is `slot ~ variable`
plot_vol <- handler |>
  aggregate(tree ~ VOLCFNET | VOLCFGRS)

head(plot_vol)
#> # Source:   SQL [?? x 7]
#> # Database: DuckDB 1.5.2 [bryce@Linux 6.17.0-29-generic:R 4.6.0//home/bryce/Programming/fiaplyr/inst/fiadb_vt_mini.duckdb]
#>   PLT_CN         STATECD COUNTYCD INVYR  PLOT VOLCFNET VOLCFGRS
#>   <chr>            <int>    <int> <int> <int>    <dbl>    <dbl>
#> 1 73597381010538      50       21  2006  1491    4027.    4573.
#> 2 73599875010538      50       21  2006  1032    1192.    1626.
#> 3 55967629010538      50       19  2004   347     367.     401.
#> 4 73599444010538      50       21  2006  1212    3154.    3574.
#> 5 62286064010538      50       27  2005   598    3867.    4388.
#> 6 73597861010538      50       21  2006  1324    3306.    3665.
```

Plot-level values are often used in statistical models and other
applications. However, some analyses do not explicitly an `aggregate`
step, such as the estimation of state-wide means or totals, so it is not
always necessary to call `aggregate`.

### Mutations

Often, custom variables are needed for analysis. These can be generated
by `mutate_tree` or `mutate_cond`, which work similarly to
`dplyr::mutate`. A canonical example is the need for basal area, a
variable that is not explicitly stored in the FIA database, but is a
simple function of diameter.

``` r
# Calculate Basal Area per acre
ba_handler <- handler |>
  mutate_tree(BA = 0.005454 * DIA^2)

plot_ba <- ba_handler |>
  aggregate(tree ~ BA)

# Verify the output
head(plot_ba)
#> # Source:   SQL [?? x 6]
#> # Database: DuckDB 1.5.2 [bryce@Linux 6.17.0-29-generic:R 4.6.0//home/bryce/Programming/fiaplyr/inst/fiadb_vt_mini.duckdb]
#>   PLT_CN         STATECD COUNTYCD INVYR  PLOT    BA
#>   <chr>            <int>    <int> <int> <int> <dbl>
#> 1 73606629010538      50       27  2006  1406  31.6
#> 2 73611154010538      50        3  2006  1210 162. 
#> 3 73616847010538      50        7  2006  1269 123. 
#> 4 73607738010538      50       27  2006  1226 153. 
#> 5 73600507010538      50       23  2006  1103 136. 
#> 6 73596558010538      50       19  2006   871  84.3
```

### Specifying Domains

Many FIA estimates take advantage of the concept of domains, which are
subsets of trees, conditions, or other inventory components. As an
example, we can make species domains by using `set_tree_domains` on the
`SPCD` column.

``` r
plot_ba_by_sp <- ba_handler |>
  set_tree_domains(SPCD) |>
  aggregate(tree ~ BA) |>
  arrange(desc(BA))

head(plot_ba_by_sp)
#> # Source:     SQL [?? x 7]
#> # Database:   DuckDB 1.5.2 [bryce@Linux 6.17.0-29-generic:R 4.6.0//home/bryce/Programming/fiaplyr/inst/fiadb_vt_mini.duckdb]
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

### Filters

Filters can be applied to the handler with `filter_tree` and
`filter_cond`. While conceptually similar to domains, `filters` discard
data, while domains merely create partitions, making computations more
efficient when data can be effectively discarded. For example, if we
merely need basal area aggregates for balsam we can use

``` r
plot_ba_balsam <- ba_handler |>
  filter_tree(SPCD == 12) |>
  aggregate(tree ~ BA) |>
  arrange(desc(BA))

head(plot_ba_balsam)
#> # Source:     SQL [?? x 6]
#> # Database:   DuckDB 1.5.2 [bryce@Linux 6.17.0-29-generic:R 4.6.0//home/bryce/Programming/fiaplyr/inst/fiadb_vt_mini.duckdb]
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
ba_est <- estimate(estimator, tree ~ BA)

ba_est
#> # A tibble: 1 × 3
#>   var   estimate    se
#>   <chr>    <dbl> <dbl>
#> 1 BA        97.5  1.71
```

By default, outputs are means, typically representing areal densities.
An estimate of the total can be made instead

``` r
ba_total_est <- estimate(estimator, tree ~ BA, output = "total")

ba_total_est
#> # A tibble: 1 × 3
#>   var     estimate        se
#>   <chr>      <dbl>     <dbl>
#> 1 BA    577059350. 10139412.
```

### Domain Estimation

Estimates respect domains applied to the handler. To form basal area
estimates by species class simply do the following

``` r
# Estimate Basal Area by Species
ba_by_sp_handle <- ba_handler |>
  set_tree_domains(SPCD)

estimator_by_sp <- PostStratifiedEstimator(ba_by_sp_handle)

ba_by_sp_est <- estimate(estimator_by_sp, tree ~ BA)

head(ba_by_sp_est)
#> # A tibble: 6 × 4
#>    SPCD var   estimate     se
#>   <dbl> <chr>    <dbl>  <dbl>
#> 1    68 BA      0.146  0.106 
#> 2    91 BA      0.397  0.177 
#> 3   901 BA      0.0457 0.0482
#> 4    94 BA      0.477  0.126 
#> 5   972 BA      0.474  0.0930
#> 6   261 BA      9.03   0.868
```

## Cautionary Results when Comparing to EVALIDator

Users are cautioned that `fiaplyr` does not implement as many guardrails
as `EVALIDator`. When users select variables in the second step of
`EVALIDator`, they are really interacting with complex queries with
side-effects that may discard certain types of trees or other inventory
components. `fiaplyr` relies on the user to explicitly discard these
themselves. At the cost of convenience, `fiaplyr` code is more explicit
about these choices, helping to generate a self-documenting intent.

As an example, let’s consider the `EVALIDator` variable called
`11009 - Gross bole bark volume of live trees (timber species at least 5 inches d.b.h.), in cubic feet, on forest land`.
Inspecting the SQL query for this we find that:

- `TREE.STATUSCD` must be `1` (live trees)
- `TREE.DIA` must be greater than or equal to `5`
- `COND.COND_STATUSCD` must be `1` (accessible forest land)
- `REF_SPECIES.WOODLAND` must be `N` (timber species)

To replicate this in `fiaplyr`, we must either (a) apply filters to the
handler or (b) specify domains that partition the data according to
these criteria. The former is more conceptually clear, but either
approach is valid. The former approach yields

``` r
handler_gross_bark <- handler |>
  filter_tree(STATUSCD == 1, DIA >= 5, WOODLAND == 'N') |>
  filter_cond(COND_STATUSCD == 1)

estimator_gross_bark <- PostStratifiedEstimator(handler_gross_bark)

est_gross_bark <- estimate(estimator_gross_bark, tree ~ VOLCFNET)

est_gross_bark
#> # A tibble: 1 × 3
#>   var      estimate    se
#>   <chr>       <dbl> <dbl>
#> 1 VOLCFNET    1547.  35.5
```

Hence, some background knowledge is needed to understand the nuance of
the mapping between `EVALIDator` variables and information needed to
construct the proper filters. Still, we believe the explicitness is a
strength, and creates code with clear intent.
