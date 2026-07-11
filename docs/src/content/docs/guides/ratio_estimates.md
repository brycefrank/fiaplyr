---
title: "Ratio Estimates"
---

The oft overlooked ratio estimator is a powerful component of FIA
analyses, enabling the estimation of ratios that can normalize estimates
to forested areas, yield averages of tree-level quantities, estimate
mortality rates, and more. An understanding of the more basic [status
estimates](status_estimates) vignette is also required.

As the name suggests, the ratio estimator is formed by dividing a
numerator estimate by a denominator estimate, making an analysis
dependent on pairs of variables. In `fiaplyr`, this is accomplished by
using a function within the `estimate` context called `ratio`, where
users specify numerator and denominator attributes. Both attributes are
evaluated from the same handler. The handler's domains apply to the
numerator and, by default, the denominator. Use `den_partitions` when the
denominator requires different domain variables.

To illustrate the power of the ratio estimator, we will provide three
examples:

1.  Growing stock volume divided by forested area.
2.  Diameter height ratio by species.
3.  Proportional representation of species.

## Growing Stock Volume divided by Forested Area

Normalizing estimates by forested area is a common practice in FIA
analyses. For example, in states with sparse forest cover, per-acre
densities that can be made with the standard post-stratified estimator
may seem small to the lay user because the density is expressed over the
entire state, rather than only for forested lands within it. The ratio
estimator resolves this by dividing the per-acre density by the
proportion of forested land, inflating the estimate to represent the
density exclusively within forests. This is straightforward using the
`ratio` helper. First, establish a handler

``` r
library(fiaplyr)
library(DBI)
library(duckdb)
library(dplyr)

# Connect to the Vermont mini database
con <- dbConnect(duckdb(), fiadb_vt_mini_path())

# Create a handler for the 2003 to 2006 evaluation for Vermont status variables
handler <- eval_handler(con, 500601)
```

Next, we subset to growing stock trees on forested land, and then
specify the `ratio` arguments. The first argument forms the output
numerators. In this case, we have not defined a partition, so this
represents just one estimate, representing the growing stock volume
across Vermont. Then, we specify the denominator as `cond()`, which
estimates the forested proportion noting that we subset to
`COND_STATUS_CD == 1` to only include forested conditions.

``` r
handler |>
  subset(tree(TREECLCD == 2), cond(COND_STATUS_CD == 1)) |>
  estimate(
    ratio(tree(VOLCFNET), cond())
  )
```

    # A tibble: 1 × 4
      var_n    var_d estimate    se
      <chr>    <chr>    <dbl> <dbl>
    1 VOLCFNET prop     1808.  41.9

Here, we see the density of growing stock per acre on forested land,
which is 1,808 cubic feet per acre. The table defines the numerator
variable `var_n`, and the denominator variable `var_d`. The point
estimate and standard error are provided.

## Diameter Height Ratio by Species

A representation of the slenderness of trees is the diameter-height
ratio, which has implications for taper, merchantability, and other
characteristics of trees. We will estimate the diameter height ratio for
growing stock trees within the 10 to 15 inch diameter class for each
species. By default, the ratio estimator generates estimates for all
possible domain pairings, but for this question it is sufficient to only
generate ratios when the species match between the numerator and
denominator, this can be done by using `domain_pairing = 'matched'`
argument.

``` r
tree_handler <- handler |>
  subset(tree(TREECLCD == 2, DIA >= 10, DIA <= 15, ACTUALHT == HT)) |>
  partition(tree(SPCD))

dhr_ests <- tree_handler |>
  estimate(
    ratio(
      tree(HT),
      tree(DIA)
    )
  ) |> filter(SPCD_n == SPCD_d)

# Prepare readable species labels
species_labels <- handler@tables$ref_species |>
  select(SPCD, COMMON_NAME) |>
  collect()

dhr <- dhr_ests |>
  arrange(desc(estimate)) |>
  filter(se != 0) |>
  left_join(species_labels, by = c("SPCD_n" = "SPCD"))

head(dhr)
```

    # A tibble: 6 × 7
      SPCD_n SPCD_d var_n var_d estimate          se COMMON_NAME      
       <dbl>  <dbl> <chr> <chr>    <dbl>       <dbl> <chr>            
    1     71     71 HT    DIA       6.93 0.000000139 tamarack         
    2    125    125 HT    DIA       6.91 0.282       red pine         
    3    901    901 HT    DIA       6.73 0.000000147 black locust     
    4    402    402 HT    DIA       6.69 0.487       bitternut hickory
    5    379    379 HT    DIA       6.24 0.779       gray birch       
    6    743    743 HT    DIA       6.20 0.289       bigtooth aspen   

Notice that we filtered the ratio estimates to only include those where
the species matched between the numerator and denominator. This is
because the ratio estimator will produce estimates for all possible
domain pairings, e.g., the diameter of `SPCD=71` divided by the height
of `SPCD=125`. This is useful in some contexts, but not here, so they
are discarded outright.

## Proportional Representation of Species

It may be of interest to estimate the proportion a species represents
within a state. This requires the denominator to omit the species domain
inherited from the handler, which can be expressed with a denominator
override.

``` r
# First specify a handler for growing stock trees, partitioned by species
prop_handler <- handler |>
  subset(tree(TREECLCD == 2)) |>
  partition(tree(SPCD))

prop_ests <- prop_handler |>
  estimate(
    ratio(
      tree(),
      tree(),
      den_partitions = list(tree())
    )
  )

prop_ests |>
  arrange(desc(estimate)) |>
  left_join(species_labels, by = c("SPCD_n" = "SPCD")) |>
  select(SPCD_n, COMMON_NAME, estimate, se) |>
  head()
```

Here, we can see the proportion of each species within the state of
Vermont for the specific case of growing stock trees. The same approach
supports other custom ratios, such as the proportion of trees with
defects or static mortality proportions.
