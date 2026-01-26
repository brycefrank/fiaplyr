
# fiaplyr <img src="inst/logo.png" align="right" height="200" />

<!-- badges: start -->
<!-- badges: end -->

`fiaplyr` provides a modern, `dplyr`-style interface for working with
Forest Inventory and Analysis (FIA) databases. It simplifies the process
of querying complex FIA database structures by abstracting away the
joins and filtering logic required to produce standard estimates and
plot-level aggregations.

## Features

- **Evaluation-Aware**: Automatically handles the complex web of filters
  required to select data for a specific FIA Evaluation (Population
  Estimation Units, Strata, Plots, Conditions, Trees).
- **Lazy Evaluation**: Uses `dbplyr` to construct queries lazily. Data
  is only pulled into R when you explicitly `collect()` it.
- **Zero-Fill**: Automatically handles missing domains (e.g., if a plot
  has no trees of a specific species, it correctly reports 0 instead of
  dropping the record).

## Installation

You can install the development version of fiaplyr from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("brycefrank/fiaplyr")
```

## Usage

### Connecting to an FIA Database

Everything starts with a connection to your FIA SQLite database.

``` r
library(fiaplyr)
library(dplyr)
library(DBI)
library(RSQLite)

# Establish a backend connection
db_path <- "C:/fia/SQLite_FIADB_OR.db"
con <- dbConnect(SQLite(), db_path)
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
plot_vol <- handler |>
  aggregate_tree(VOLCFNET)

# The result is a lazy query. Collect to execute.
head(collect(plot_vol))
#> # A tibble: 6 × 6
#>   PLT_CN         STATECD INVYR  PLOT COUNTYCD VOLCFNET
#>   <chr>            <int> <int> <int>    <int>    <dbl>
#> 1 23871905010900      41  2001 65609       43    6937.
#> 2 23872085010900      41  2001 92815       43    4165.
#> 3 23872244010900      41  2001 60220       43    1294.
#> 4 23872460010900      41  2001 54379       43       0 
#> 5 23872654010900      41  2001 57837       43    4190.
#> 6 23872899010900      41  2001 65233       43       0
```

### Grouping and Mutations

You can perform more complex analyses by grouping (defining domains) and
creating new variables on the fly.

``` r
# Calculate Basal Area per acre by Species
plot_ba_by_sp <- handler |>
  set_tree_domains(SPCD) |> # Group by Species Code
  mutate_tree(BA = 0.005454 * DIA^2) |> # Calculate Basal Area per tree
  aggregate_tree(BA) # Sum to plot level (weighted by TPA)

# Verify the output
# Note: This will result in multiple rows per plot (one for each species present)
head(collect(plot_ba_by_sp))
#> # A tibble: 6 × 7
#>   PLT_CN         STATECD INVYR  PLOT COUNTYCD  SPCD    BA
#>   <chr>            <int> <int> <int>    <int> <dbl> <dbl>
#> 1 23871905010900      41  2001 65609       43    11     0
#> 2 23871905010900      41  2001 65609       43    15     0
#> 3 23871905010900      41  2001 65609       43    17     0
#> 4 23871905010900      41  2001 65609       43    19     0
#> 5 23871905010900      41  2001 65609       43    20     0
#> 6 23871905010900      41  2001 65609       43    21     0
```
