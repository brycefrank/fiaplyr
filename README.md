
# fiaplyr <img src="inst/logo.png" align="right" height="200" />

<!-- badges: start -->
<!-- badges: end -->

`fiaplyr` provides a modern, `dplyr`-style interface for working with
Forest Inventory and Analysis (FIA) databases. It simplifies the process
of querying complex FIA database structures by abstracting away the
joins and filtering logic required for officially recognized
“Evaluations” (EVALIDs).

With `fiaplyr`, you can chain together operations to compute plot-level
estimates (like volume per acre) while offloading the heavy lifting to
the database.

## Features

- **Evaluation-Aware**: Automatically handles the complex web of filters
  required to select data for a specific FIA Evaluation (Population
  Estimation Units, Strata, Plots, Conditions, Trees).
- **Lazy Evaluation**: Uses `dbplyr` to construct queries lazily. Data
  is only pulled into R when you explicitly `collect()` it.
- **Tidy Syntax**: Use familiar verbs like `summarize_tree`,
  `summarize_cond`, `mutate_tree`, and `mutate_cond`.
- **Zero-Fill**: Automatically handles missing domains (e.g., if a plot
  has no trees of a specific species, it correctly reports 0 instead of
  dropping the record).

## Installation

You can install the development version of fiaplyr from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("yourusername/fiaplyr")
```

## Usage

### Connecting to an FIA Database

Everything starts with a connection to your FIA SQLite database.

``` r
devtools::load_all() # Load fiaplyr package from local source
library(dplyr)
library(DBI)
library(RSQLite)

# Establish a backend connection
db_path <- "./db/fiadb_or.duckdb"
con <- dbConnect(duckdb::duckdb(), db_path)
```

### The Evaluation Handler

The `eval_handler` is the core object. It connects to the database and
sets up the scope for a specific Evaluation ID (EVALID).

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
#> Estn Units:      3 
#> Strata:          183 
#> Plots:           14816 
#> Inventory Years: 2001 - 2010 
#> Measure Years:   1999 - 2010
```

### Basic Aggregation

Calculate plot-level summaries easily. `fiaplyr` automatically weights
tree variables by `TPA_UNADJ` (Trees Per Acre) during aggregation.

``` r
# Calculate Net Cubic Foot Volume per acre for each plot
# The formula syntax is `slot ~ variable`
plot_vol <- handler |>
  aggregate(tree ~ VOLCFNET)

# The result is a lazy query. Collect to execute.
head(collect(plot_vol))
#> # A tibble: 6 × 6
#>   PLT_CN         STATECD INVYR  PLOT COUNTYCD VOLCFNET
#>   <chr>            <dbl> <dbl> <dbl>    <dbl>    <dbl>
#> 1 23872244010900      41  2001 60220       43    1294.
#> 2 23873740010900      41  2001 79818       43    4097.
#> 3 23874026010900      41  2001 71355       43    8672.
#> 4 24002743010900      41  2002 74149        1    1153.
#> 5 41118343010497      41  2001 86183       51   14359.
#> 6 23893265010900      41  2001 93622       35     596.
```

### Grouping and Mutations

You can perform more complex analyses by grouping (defining domains) and
creating new variables on the fly.

``` r
# Calculate Basal Area per acre by Species
plot_ba_by_sp <- handler |>
  set_tree_domains(SPCD) |> # Group by Species Code
  mutate_tree(BA = 0.005454 * DIA^2) |> # Calculate Basal Area per tree
  aggregate(tree ~ BA) # Sum to plot level (weighted by TPA)

# Verify the output
# Note: This will result in multiple rows per plot (one for each species present)
head(collect(plot_ba_by_sp))
#> # A tibble: 6 × 7
#>   PLT_CN         STATECD INVYR  PLOT COUNTYCD  SPCD    BA
#>   <chr>            <dbl> <dbl> <dbl>    <dbl> <dbl> <dbl>
#> 1 41118812010497      41  2002 74834       33   361  3.43
#> 2 23880730010900      41  2001 96358       29   361 16.2 
#> 3 23885099010900      41  2001 88260       33   361  6.36
#> 4 13232110010497      41  2001 61582       33   361  8.42
#> 5 24055829010900      41  2002 57767       29   361  5.01
#> 6 41118510010497      41  2001 51975       29   361  6.46
```

## Estimation

`fiaplyr` supports design-based estimation using post-stratification.

### Post-Stratified Estimator

To produce population estimates (e.g., total volume, total area), use
the `PostStratifiedEstimator` class.

``` r
# Create an estimator from the handler
estimator <- PostStratifiedEstimator(handler)

# Estimate total Net Cubic Foot Volume
# The formula syntax is `slot ~ variable`
vol_est <- estimate(estimator, tree ~ VOLCFNET)

head(vol_est)
#> # Source:   SQL [1 x 1]
#> # Database: DuckDB v1.3.0 [bfran@Windows 10 x64:R 4.4.0/C:\Users\bfran\programming\fiaplyr\db\fiadb_or.duckdb]
#>   VOLCFNET
#>      <dbl>
#> 1    1751.
```

### Domain Estimation

Estimates respect any grouping (domains) applied to the handler.

``` r
# Estimate Volume by Species
vol_by_sp_est <- handler |>
  set_tree_domains(SPCD) |>
  PostStratifiedEstimator() |>
  estimate(tree ~ VOLCFNET)

head(vol_by_sp_est)
#> # Source:   SQL [6 x 2]
#> # Database: DuckDB v1.3.0 [bfran@Windows 10 x64:R 4.4.0/C:\Users\bfran\programming\fiaplyr\db\fiadb_or.duckdb]
#>    SPCD VOLCFNET
#>   <dbl>    <dbl>
#> 1   760   0.0301
#> 2   361  12.8   
#> 3   264  53.2   
#> 4   542   1.67  
#> 5   108  52.8   
#> 6   299   2.40
```
