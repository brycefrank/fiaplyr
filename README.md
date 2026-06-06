
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

``` r
devtools::load_all() # Load fiaplyr package from local source
library(dplyr)
library(DBI)
library(RSQLite)

# Establish a backend connection
db_path <- "./db/SQLite_FIADB_OR.db"
con <- dbConnect(RSQLite::SQLite(), db_path)
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
#> # A tibble: 6 × 2
#>   EVALID EVAL_DESCR                                                             
#>    <int> <chr>                                                                  
#> 1 411000 OREGON 2010: 2001-2010: ALL AREA                                       
#> 2 411001 OREGON 2010: 2001-2010: CURRENT AREA, CURRENT VOLUME                   
#> 3 411007 OREGON 2010: 2001-2010: DWM                                            
#> 4 411700 OREGON 2017: 2008-2017: ALL AREA                                       
#> 5 411701 OREGON 2017: 2008-2017: CURRENT AREA, CURRENT VOLUME                   
#> 6 411703 OREGON 2017: 2001-2007 to 2011-2017: AREA CHANGE, GROWTH, REMOVALS, MO…
```

we will use `411001`, which covers Oregon (`41`) for 2001-2010 (`10`),
and can be used to estimate status variables (`01`).

``` r
# Initialize handler for EVALID 411001
handler <- eval_handler(con, 411001)

# Inspect the handler summary
handler
#> EvalHandler
#> ----------
#> EVALID:          411001 
#> Description:     OREGON 2010: 2001-2010: CURRENT AREA, CURRENT VOLUME
#> 
#> Plots:           14816 
#> Inventory Years: 2001 - 2010 
#> Measure Years:   1999 - 2010
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
#> # Database: sqlite 3.53.1 [/home/bryce/Programming/fiaplyr/db/SQLite_FIADB_OR.db]
#>   PLT_CN         STATECD COUNTYCD INVYR  PLOT VOLCFNET VOLCFGRS
#>   <chr>            <int>    <int> <int> <int>    <dbl>    <dbl>
#> 1 23904592010900      41        5  2001 54961    5027.    5134.
#> 2 23904900010900      41        5  2001 55427       0        0 
#> 3 23904948010900      41        5  2001 55784   12483.   14352.
#> 4 23905637010900      41        5  2001 57362       0        0 
#> 5 23903017010900      41        5  2001 59819   13402.   14228.
#> 6 41118129010497      41        5  2001 61868    3282.    3384.
```

Plot-level values are often used in remote sensing models and other
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
#> # Database: sqlite 3.53.1 [/home/bryce/Programming/fiaplyr/db/SQLite_FIADB_OR.db]
#>   PLT_CN         STATECD COUNTYCD INVYR  PLOT    BA
#>   <chr>            <int>    <int> <int> <int> <dbl>
#> 1 23904592010900      41        5  2001 54961  199.
#> 2 23904900010900      41        5  2001 55427    0 
#> 3 23904948010900      41        5  2001 55784  400.
#> 4 23905637010900      41        5  2001 57362    0 
#> 5 23903017010900      41        5  2001 59819  422.
#> 6 41118129010497      41        5  2001 61868  165.
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
#> # Database:   sqlite 3.53.1 [/home/bryce/Programming/fiaplyr/db/SQLite_FIADB_OR.db]
#> # Ordered by: desc(BA)
#>   PLT_CN          STATECD COUNTYCD INVYR  PLOT  SPCD    BA
#>   <chr>             <int>    <int> <int> <int> <dbl> <dbl>
#> 1 29882679010497       41       39  2009 57304   202  706.
#> 2 29395271010497       41       39  2008 88646   202  677.
#> 3 29395087010497       41       39  2008 67285   202  632.
#> 4 41120103010497       41       39  2005 74184   202  596.
#> 5 193209035020004      41       39  2006 74101   202  581.
#> 6 29881767010497       41       19  2009 57209   202  570.
```

### Filters

Filters can be applied to the handler with `filter_tree` and
`filter_cond`. While conceptually similar to domains, `filters` discard
data, while domains merely create partitions, making computations more
efficient when data can be effectively discarded. For example, if we
merely need basal area aggregates for Douglas-fir we can use

``` r
plot_ba_douglas_fir <- ba_handler |>
  filter_tree(SPCD == 202) |>
  aggregate(tree ~ BA) |>
  arrange(desc(BA))

head(plot_ba_douglas_fir)
#> # Source:     SQL [?? x 6]
#> # Database:   sqlite 3.53.1 [/home/bryce/Programming/fiaplyr/db/SQLite_FIADB_OR.db]
#> # Ordered by: desc(BA)
#>   PLT_CN          STATECD COUNTYCD INVYR  PLOT    BA
#>   <chr>             <int>    <int> <int> <int> <dbl>
#> 1 29882679010497       41       39  2009 57304  706.
#> 2 29395271010497       41       39  2008 88646  677.
#> 3 29395087010497       41       39  2008 67285  632.
#> 4 41120103010497       41       39  2005 74184  596.
#> 5 193209035020004      41       39  2006 74101  581.
#> 6 29881767010497       41       19  2009 57209  570.
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
#> 1 BA        65.7 0.457
```

By default, outputs are means, typically representing areal densities.
An estimate of the total can be made instead

``` r
ba_total_est <- estimate(estimator, tree ~ BA, output = "total")

ba_total_est
#> # A tibble: 1 × 3
#>   var      estimate        se
#>   <chr>       <dbl>     <dbl>
#> 1 BA    4142806434. 28768504.
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
#> 1    11 BA      1.16   0.0870
#> 2    15 BA      2.51   0.117 
#> 3    17 BA      2.81   0.103 
#> 4    19 BA      0.650  0.0580
#> 5    20 BA      0.0631 0.0199
#> 6    21 BA      0.777  0.0912
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
#> 1 VOLCFNET    1575.  16.0
```

Hence, some background knowledge is needed to understand the nuance of
the mapping between `EVALIDator` variables and information needed to
construct the proper filters. Still, we believe the explicitness is a
strength, and creates code with clear intent.
