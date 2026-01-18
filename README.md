
# fiaplyr

An FIA database toolkit based on dplyr-style grammar. Users chain
together common operations (e.g., plot-level summaries, estimates, etc)
offloading computations to the database. When ready, users `collect`
data locally. This offers flexible ways to interact with *evaluations*,
which are official collections of plots and conditions that are used to
produce estimates.

``` r
library(fiaplyr)
library(dplyr)

# Establish a backend connection
db_path <- "C:/fia/SQLite_FIADB_OR.db"
con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

# Pass an evaluation id to a handler and print a summary
handler <- eval_handler(con, 411001)

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

## Plot-level Aggregations

We can aggregate variables to the plot level

``` r
plot_query <- handler |>
  summarize_plot(VOLCFNET)
```

This makes a `dbplyr` query, which we can `collect` to retrieve local
data

``` r
collect(plot_query)
#> # A tibble: 14,816 × 6
#>    CN             STATECD INVYR  PLOT COUNTYCD VOLCFNET
#>    <chr>            <int> <int> <int>    <int>    <dbl>
#>  1 23871905010900      41  2001 65609       43    6937.
#>  2 23872085010900      41  2001 92815       43    4165.
#>  3 23872244010900      41  2001 60220       43    1294.
#>  4 23872460010900      41  2001 54379       43       0 
#>  5 23872654010900      41  2001 57837       43    4190.
#>  6 23872899010900      41  2001 65233       43       0 
#>  7 23872923010900      41  2001 56766       43       0 
#>  8 23872947010900      41  2001 62250       43       0 
#>  9 23872971010900      41  2001 73863       43    1140.
#> 10 23873131010900      41  2001 65403       43    1277.
#> # ℹ 14,806 more rows
```
