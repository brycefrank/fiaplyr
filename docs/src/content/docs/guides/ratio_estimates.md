---
title: "Ratio Estimates"
---

The oft overlooked ratio estimator is a powerful component of FIA analyses,
enabling the estimation of ratios that can normalize estimates to forested
areas, yield averages of tree-level quantities, estimate mortality rates,
and more. Users are encouraged to read the material of [@scott etc chapter] for
useful examples regarding the ratio estimator before proceeding. An
understanding of the more basic [status estimates](status_estimates)
vignette is also required.

As the name suggests, the ratio estimator is formed by dividing a numerator
estimate by a denominator estimate, making an analysis dependent on pairs of
variables. In `fiaplyr`, we accomplish this by passing two handler objects to
the estimator, one that specifies all estimates in the numerator, and another
that specifies all estimates in the denominator. Then, estimates for all
possible pairings are made.

Recall that the post-stratified estimator used in the status vignette produces
$d \cdot v$ estimates, where $d$ is the total number of distinct domains, and
$v$ is the number of variables. If we take this as the numerator handler, and
relabel the notation with the subscript $n$, we have $d_n \cdot v_n$ estimates.
Likewise, the denominator handler produces $d_d \cdot v_d$ estimates.  The ratio
estimator then produces $d_n \cdot v_n \cdot d_d \cdot v_d$ estimates, as all
possible pairings of numerator and denominator estimates are made. This creates
an extremely expansive set of estimates for the user to interact with, and can
be overwhelming at times. To reduce the complexity of the output, we suggest
using filtering functions (`filter_tree`, `filter_cond`, etc.), rather than
relying on granular domains.

To illustrate the power of the ratio estimator, we will estimate 
