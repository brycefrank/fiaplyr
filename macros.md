Currently, we use implicit macros when aggregating. For example

```{r}
handler |>
  aggregate(tree(VOLCFGRS))
```

means something like: using TPA_UNADJ generated a weighted sum of VOLCFGRS
across all trees in the plot: sum(TPA_UNADJ * VOLCFGRS) (roughly). This is
fantastic default behavior. However, we want to enable the use of other "macros"
that users can provide, much in the same way that dplyr summarise works. For
example, we may want a weighted mean of the form sum(TPA_UNADJ * VOLCFGRS) / sum(TPA_UNADJ)
there is currently no way to do this.

Likewise

```{r}
handler |>
  aggregate(cond())
```

is sum(CONDPROP_UNADJ) (roughly).