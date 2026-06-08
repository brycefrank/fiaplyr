

We want to generate marginal totals. Consider `PostStratifiedEstimator`, if we
defined two domain variables: SPCD, COND, we get a table of estimates, one row
indicating the SPCD, one indicating the COND, and another with the estimate and
a final one with the SE.

However, we also want marginal estimates. For example:
    - The total within COND "A" across all species...etc for all COND values
    - The total within Species "A" across all conds...etc for all SPCD values
    - The grand total.

To accomplish this we need to specify how to make the estimates and the proper
standard errors. This is straightforward to do: internally maniupalte the
domains to get the estimates we desire. However, I worry about computational
efficiency. Examine how one might do this. I can see two options:

1. Manipulate the domains at the tree/condition level etc, then run them through
the standard estimation pipeline. Fine with this, cool.
2. Add the estimates themselves together. However, we would need to handle the
covariances of estimates to get the proper SE. This is probably nasty in terms
of computations as well.