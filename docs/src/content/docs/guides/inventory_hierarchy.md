---
title: "Subsetting the Inventory Hierarchy"
---

The FIA inventory is organized in a hierarchical manner, and effective
use of `fiaplyr`, requires a basic understanding of this concept. While
many variations of the inventory hierarchy exists, a useful illustration
is the following:

``` text
- evaluation
  - plot
    - condition
      - tree
      - downed woody material
      - seedling
      - non-tree vegetation
      - soil core
      - etc.
```

When users initiate with `eval_handler()`, they are creating a handler
for a specific evaluation, and interact in various ways with this
hierarchy. In particular, `subset()` allows users to discard certain
parts of the above hierarchy to refine an analysis.

For example, if a user is only interested in Douglas-fir trees
(`SPCD == 202`), they can do the following:

``` r
handler <- eval_handler(con, 500601) |>
  subset(tree(SPCD == 202))
```

The interpretation here is fairly simple, all trees that are not
Douglas-fir are discarded from further analysis (i.e., `estimate` or
`aggregate`). Consider the following case, where conditions are subset
instead

``` r
handler <- eval_handler(con, 500601) |>
  subset(cond(COND_STATUS_CD == 1))
```

This case is more complex, because the subsetting applies to conditions
as well as all of the child components (e.g., trees, seedlings, etc.).
Thus, any conditions that are not forested (`COND_STATUS_CD == 1`) are
discarded, and all trees, seedlings, etc. that are associated with those
conditions are also discarded. Thus, subsetting should be viewed as a
way to trim all downstream components of the inventory hierarchy.

## Subsetting Plots and Sample Immutability

In `fiaplyr` a sample, i.e., the set of plots associated with the
handler, is immutable in the sense that the sample size remains fixed
throughout the use of the handler. Yet, users are free to use `subset`
with a `plot` helper. Consider the following:

``` r
handler <- eval_handler(con, 500601) |>
  subset(plot(COUNTYCD == 1))
```

In this case, all conditions (and children of those conditions) are
discarded that are not in COUNTYCD 1, but the sample size of the handler
remains the same. In general, we recommend avoiding subsetting with
`plot` helpers, as it can lead to confusion, but some important use
cases exist, especially when the plot table contains information that is
not available in the condition or lower tables.
