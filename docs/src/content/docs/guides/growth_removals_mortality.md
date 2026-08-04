---
title: "Growth, Removals, and Mortality"
---

Growth, removals, and mortality (GRM) refers to a set of estimation
procedures used by the FIA to quantify forest change. Broadly, trees are
assigned to change classes, which we refer to as transitions. Then,
estimates are constructed based on the transition and desired output of
the user. Typical outputs include estimates of mortality volume,
removals, and others. Transitions are dependent on a pre-specified
`tree_basis` and `land_basis`. For example, ingrowth with respect to
growing stock is more stringent than ingrowth with respect to all live
trees, because growing stock requires a larger minimum diameter and more
strict defect constraints. Thus, users must specify the tree and land
bases, which will facilitate `fiaplyr` in correctly applying the
appropriate transition rules, which is done when the handler is
initialized.

Generally, users produce estimates using a series of facilitating
functions called macros, which are called within `aggregate` and
`estimate` contexts. The macro names should be familiar, and are all
prefixed with `grm_`. Common examples include:

- `grm_mortality`
- `grm_removal`
- `grm_ingrowth`
- `grm_net_change`

and others. Users supply the desired variable to the macro, whether or
not to annualize the estimate, and any other relevant arguments. Aside
from the macros, analysis of GRM data is largely similar to that of
status analysis. We animate GRM analysis with an example from Vermont.

## Specifying a Handler

To begin with GRM analyses, specify a handler as always. However, this
handler must use an evaluation ending in `3` which indicates a
GRM-specific evaluation. We also pass the `GRMAnalysis` specification.

``` r
library(fiaplyr)
library(DBI)
library(duckdb)
library(dplyr)

con <- dbConnect(duckdb(), fiadb_vt_mini_path())
handler <- eval_handler(con, 501103, spec = grm_analysis())

handler
```

    EvalHandler
    ----------
    EVALID:          501103 
    Description:     VERMONT 2011: 2003-2007 to 2008-2011: AREA CHANGE, GROWTH,
                     REMOVALS, MORTALITY

    Plots:           770 

    GRM Spec
    Tree basis:      all_live 
    Land basis:      forest_land 
    Rules:           8 

Here, we see the handler is initialized with a GRM evaluation, and the
summary statement gives particulars. The specification defaults to a
tree basis of `all_live` and a land basis of `forest_land`, but these
can be changed by setting the `tree_basis` and `land_basis` arguments in
the `grm_analysis()` function. Options for the tree basis include
`all_live`, `growing_stock`, and `sawtimber` and options for the land
basis include `forest_land` and `timberland`.

The core table involved with GRM analyses is the `tree_history` table,
which contains the tree-level histories for each tree in the
remeasurement period, and relies on the GRM paradigm of beginning,
midpoint, and end values. Users can view the lazy query for the
`tree_history` table with

``` r
handler@tables$tree_history |>
  select(DIA_begin, DIA_midpt, DIA) |>
  head()
```

    # Source:   SQL [?? x 3]
    # Database: DuckDB 1.4.4 
      DIA_begin DIA_midpt   DIA
          <dbl>     <dbl> <dbl>
    1       8.3      8.65   9  
    2      24.4     24.8   25.3
    3       5.2      5.35   5.5
    4      13.2     13.6   13.9
    5       2        2.2    2.4
    6       1.6      1.65   1.7

Notice that the beginning and midpoint values have suffixes, while the
end does not. This complies with typical FIA conventions.

## Tree Transitions

In the GRM paradigm, trees undergo transitions, primarily based on their
life history. For example, a tree that is measured and alive at the
beginning of the period may survive to the endpoint, wherein it is
classified as a survivor tree. Tree transitions are applied when the
handler is initialized, using transition rules that comply with the
selected tree and land bases specified in the `grm_analysis()` function.
Users should interpret transitions as a column in the `tree_history`
table that contains the transition type, for which we use the
conventional names from the FIA database, but in lower case to
differentiate from the original values. While it is important to be
aware of the transitions, they are typically abstracted away when using
GRM macros. Still, some inspection is useful, such as a count of trees
by transition

``` r
handler@tables$tree_history |>
  group_by(transition) |>
  summarise(n = n()) |>
  arrange(transition) |>
  print(n = 10)
```

    # Source:     SQL [?? x 2]
    # Database:   DuckDB 1.4.4 
    # Ordered by: transition
       transition     n
       <chr>      <dbl>
     1 cut1         413
     2 cut2           3
     3 diversion1    96
     4 ingrowth    1456
     5 mortality1   959
     6 not_used    7082
     7 reversion1    16
     8 reversion2   182
     9 survivor   15369
    10 unknown       80

## Estimation of Change Components with Macros

Change components refer to the various outputs that users may be
interested in, such as mortality volume, removals, and others. For most
use cases, it is sufficient to use the GRM macros to specify the desired
change components, which abstract away complex transition rules and
other accounting methods.

Beginning with a simple example, let’s assume we want to estimate the
total mortality volume. The macro `grm_mortality` is used, which
generates mortality for the specified variable. The `annualize` argument
is used to produce annual mortality rates, as is done in most standard
FIA estimates.

``` r
handler |>
  estimate(
    tree_history(mort = grm_mortality(VOLCFSND, annualize = TRUE)),
    output = "total"
  )
```

    # Source:   SQL [?? x 3]
    # Database: DuckDB 1.4.4 
      var     estimate       se
      <chr>      <dbl>    <dbl>
    1 mort  101032246. 6741699.

Likewise, ingrowth can also be estimated with the `grm_ingrowth` macro

``` r
handler |>
  estimate(
    tree_history(ingrowth = grm_ingrowth(VOLCFSND, annualize = TRUE)),
    output = "total"
  )
```

    # Source:   SQL [?? x 3]
    # Database: DuckDB 1.4.4 
      var       estimate       se
      <chr>        <dbl>    <dbl>
    1 ingrowth 31024427. 1391141.

Higher order change components are `grm_net_change`, `grm_net_growth`
and `grm_gross_growth`, all of which use convenient macros.

``` r
handler |>
  estimate(
    tree_history(net_change = grm_net_change(VOLCFSND, annualize = TRUE)),
    output = "total"
  )
```

    # Source:   SQL [?? x 3]
    # Database: DuckDB 1.4.4 
      var          estimate        se
      <chr>           <dbl>     <dbl>
    1 net_change 160580318. 20799837.

## Estimation of Ratios Involving Change

Change estimates also need to be expressed as ratios in many settings,
especially when normalizing to forested area. Estimates of this kind
proceed along the same lines as [ratio estimates](../ratio_estimates/).
The only modification, of course, is the specification of the GRM
estimates themselves.

``` r
handler |>
  subset(cond(COND_STATUS_CD == 1)) |>
  estimate(
    ratio(
      tree_history(mort = grm_mortality(VOLCFSND, annualize = TRUE)),
      cond()
    )
  )
```

    # Source:   SQL [?? x 4]
    # Database: DuckDB 1.4.4 
      var_n var_d estimate    se
      <chr> <chr>    <dbl> <dbl>
    1 mort  prop      22.0  1.46
