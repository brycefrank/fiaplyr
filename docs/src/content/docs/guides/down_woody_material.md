---
title: "Down Woody Material"
---

Down woody material (DWM) refers to a set of attributes involving the
volume, biomass, and other characteristics of detritus on the forest
floor. DWM analysis involves particular field methods including line
transect sampling, among others, that require special consideration when
creating plot-level values. Currently, `fiaplyr` supports DWM analysis
up to, but no further than, a pre-compiled DWM table referred to as
`COND_DWM_CALC`. This table is prepared by FIA, and standardizes the DWM
attributes for each condition in the FIA database. However, use of this
table is fairly restrictive, preventing e.g., piece-level domains for
coarse woody debris and other creative ideas. Still, basic analysis of
DWM is possible, and we demonstrate this with an example from Vermont.

Much like the GRM case, users produce estimates using a series of
facilitating functions called macros, which are called within
`aggregate` and `estimate` contexts. The macro names should be familiar,
and are all prefixed with `dwm_`. Common examples include:

- `dwm_cwd`
- `dwm_fwd`
- `dwm_litter`
- `dwm_pile`

These macros correspond to different components of DWM analysis, and the
supported variables differ by component, as shown in the following
table:

| Component           | Macro          | Supported variables                |
|---------------------|----------------|------------------------------------|
| Coarse woody debris | `dwm_cwd()`    | `VOLCF`, `DRYBIO`, `CARBON`, `LPA` |
| Fine woody debris   | `dwm_fwd()`    | `VOLCF`, `DRYBIO`, `CARBON`        |
| Residual piles      | `dwm_pile()`   | `VOLCF`, `DRYBIO`, `CARBON`        |
| Fuels               | `dwm_fuel()`   | `DRYBIO`, `CARBON`                 |
| Duff                | `dwm_duff()`   | `DRYBIO`, `CARBON`                 |
| Litter              | `dwm_litter()` | `DRYBIO`, `CARBON`                 |

Units are cubic feet per acre for `VOLCF`, dry short tons per acre for
`DRYBIO`, short tons per acre for `CARBON`, and pieces per acre for
`LPA`. `dwm_fwd()` additionally requires a `size` argument of `"SM"`,
`"MD"`, `"LG"`, or `"ALL"` (the last summing all three classes).

## Specifying a Handler

To begin with DWM analyses, specify a handler as always. However, this
handler must use an evaluation ending in `7`, which indicates a
DWM-specific evaluation. We also pass the `dwm_analysis()`
specification.

``` r
library(fiaplyr)
library(DBI)
library(duckdb)
library(dplyr)

con <- dbConnect(duckdb(), fiadb_vt_mini_path())
handler <- eval_handler(con, 501007, spec = dwm_analysis())

handler
```

    EvalHandler
    ----------
    EVALID:          501007 
    Description:     VERMONT 2010: 2006-2010: DWM

    Plots:           58 
    Inventory Years: 2006 - 2010 
    Measure Years:   2006 - 2010 

Here, we see the handler is initialized with a DWM evaluation, and the
summary statement gives particulars.

## Estimation of Down Woody Material Components

Beginning with a simple example, let’s assume we want to estimate the
total coarse woody debris biomass. The macro `dwm_cwd` is used, which
can accept the `DRYBIO` variable, yielding

``` r
handler |>
  estimate(
    dwm(
      cwd_bio = dwm_cwd(DRYBIO)
    ),
    output = "total"
  ) |>
  collect()
```

    # A tibble: 1 × 3
      var      estimate       se
      <chr>       <dbl>    <dbl>
    1 cwd_bio 24318923. 4263451.

Likewise, the biomass of other components can be estimated with the
appropriate macros, such as `dwm_fwd`, which accepts a size class
argument

``` r
handler |>
  estimate(
    dwm(
      fwd_sm_bio = dwm_fwd(DRYBIO, size = "SM")
    ),
    output = "total"
  ) |>
  collect()
```

    # A tibble: 1 × 3
      var        estimate     se
      <chr>         <dbl>  <dbl>
    1 fwd_sm_bio  342676. 36070.

## Plot-Level Aggregation

Before estimation, plot-level aggregates can be obtained with
`aggregate()`. Unlike `estimate()`, aggregation operates on the `_UNADJ`
DWM fields and returns one row per plot:

``` r
handler |>
  aggregate(dwm(dwm_cwd(VOLCF))) |>
  collect() |>
  head()
```

    # A tibble: 6 × 6
      PLT_CN          STATECD COUNTYCD INVYR  PLOT dwm_cwd_VOLCF
      <chr>             <int>    <int> <int> <int>         <dbl>
    1 118137344010661      50       25  2008  1130          141.
    2 118137372010661      50       27  2008   635          231.
    3 118137390010661      50        3  2008   405          737.
    4 118137426010661      50        5  2008  1129          665.
    5 118137460010661      50       17  2008    33          341.
    6 118137512010661      50       19  2008  1484            0 

Like `estimate()`, `aggregate()` returns a lazy query; `collect()` is
used above to bring the result into memory.

## Estimation of Ratios Involving Down Woody Material

DWM estimates often need to be expressed as ratios, especially when
normalizing to forested area. Estimates of this kind proceed along the
same lines as [ratio estimates](../ratio_estimates/). The only
modification, of course, is the specification of the DWM estimates
themselves.

``` r
handler |>
  subset(cond(COND_STATUS_CD == 1)) |>
  estimate(
    ratio(
      dwm(dwm_cwd(CARBON)),
      cond()
    )
  ) |>
  collect()
```

    # A tibble: 1 × 4
      var_n          var_d estimate    se
      <chr>          <chr>    <dbl> <dbl>
    1 dwm_cwd_CARBON prop      2.72 0.461
