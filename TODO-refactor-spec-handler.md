# Handler/Spec Separation Refactor

Track progress on separating the **handler** (plot selection) from the
**analysis spec** (aggregation policy). This unlocks composing
`window_handler` with any analysis spec.

## The core problem

`initialize_tables(spec, db, evalid, backend)` (in each spec) conflates two
concerns:

1. **Plot selection** (handler work): pop frame
   (`POP_EVAL` -> `POP_ESTN_UNIT` -> `POP_STRATUM` -> `POP_PLOT_STRATUM_ASSGN`)
   decides which plots belong to the analysis.
2. **Data-structure building** (spec work): the child tables the aggregation
   needs (cond/tree for status; + tree_history for GRM; + cond_dwm_calc for
   DWM).

Mental model we want:

- **Handler** = *which plots* + pipeline (`subset`/`transform`/`augment`/
  `partition`).
  - `EvalHandler`: pop-frame selection (keeps pop tables = eval context).
  - `WindowHandler`: spatial/temporal window selection (no pop tables).
- **Spec** = *how to aggregate* that plot set: valid scopes, child tables,
  `.aggregate_combined` machinery.

These are **orthogonal** — we should be able to compose, e.g.,
`window_handler(...) + grm_analysis()` (growth/removals inside a county
polygon). The current design can't express that.

## Concrete steps (minimal path)

- [x] Change the spec table-building contract:
      `initialize_tables(spec, db, evalid, backend)` ->
      `build_tables(spec, plot_qry, db, backend, evalid = NULL)`
- [x] Move plot selection into the handler constructors:
  - [x] `eval_handler`: pop frame -> selected plots (retain pop tables for
        estimation context)
  - [x] `window_handler`: window -> selected plots
- [x] Add a `spec` slot to `WindowHandler`; default
      `window_handler(..., spec = status_analysis())`
- [x] Register shared verb methods on `WindowHandler`
      (`aggregate`, `subset`, `transform`, `partition`, `augment`,
      `materialize`); shared impls in `R/EvalHandler.R` dispatch on both
      handler classes
- [x] Decide whether a new lightweight spec (e.g. `window_analysis()`) is
      needed or whether existing specs suffice once `initialize_tables` is
      unbundled (the user says: we SHOULD NOT have a lightweight spec for this
      imo) -> existing specs suffice; no new spec needed
- [x] Verify `aggregate()` on a `WindowHandler` works with each spec
      (status: tree/cond + verbs; GRM: tree/tree_history; DWM: explanatory
      error at construction)
- [x] Update docs (vignettes/guides, reference index) for the new composition
      (roxygen man pages + docs reference rebuilt; `window_handler()` example
      shows GRM composition)

## Deferred / riders

- [ ] **Estimation for `WindowHandler`** is a separate question: a window has
      no pop frame, so post-stratified estimation can't apply. Needs its own
      estimator story (user-supplied weights or different estimators). (user
      says that we will eventually do this, and should have a sensible way to
      define estimators for this purpose and, potentially these estimators
      could be shared with eval_handler (e.g., ones that dont use
      post-stratification)
- [ ] **Maximal version**: unify `EvalHandler`/`WindowHandler` under a common
      base class so verb methods dispatch once instead of being duplicated.
- [ ] **DWM nuance**: `COND_DWM_CALC` is `EVALID`-keyed, so
      `window_handler + dwm_analysis()` needs an eval context. Done: DWM
      `build_tables()` aborts with an explanatory error when `evalid` is
      `NULL` (class `fiaplyr_dwm_requires_evalid`).

## Status

Not committed (tracking file only).
