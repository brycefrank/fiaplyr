---
title: "Full API Index"
description: "Auto-generated reference pages from fiaplyr Rd documentation."
---


## Handlers

Create handlers for FIA database analyses.

- [`eval_handler`](./eval_handler/)

## Handler Methods

Subset, transform, estimate, and summarize FIA data.

- [`aggregate`](./aggregate/)
- [`augment`](./augment/)
- [`estimate`](./estimate/)
- [`estimate_ratio`](./estimate_ratio/)
- [`evalid`](./evalid/)
- [`materialize`](./materialize/)
- [`partition`](./partition/)
- [`ratio`](./ratio/)
- [`show`](./show/)
- [`subset`](./subset/)
- [`summary`](./summary/)
- [`transform`](./transform/)

## Analysis Specifications

Configure the analysis context used by a handler.

- [`grm_analysis`](./grm_analysis/)
- [`status_analysis`](./status_analysis/)
- [`dwm_analysis`](./dwm_analysis/)

## Scoped Helpers

Target particular tables within a handler.

- [`fiadb_vt_mini_path`](./fiadb_vt_mini_path/)
- [`set_fiaplyr_verbosity`](./set_fiaplyr_verbosity/)
- [`cond`](./cond/)
- [`pcond`](./pcond/)
- [`plot`](./plot/)
- [`pplot`](./pplot/)
- [`ptree`](./ptree/)
- [`PostStratifiedEstimator`](./poststratifiedestimator/)
- [`PostStratifiedRatioEstimator`](./poststratifiedratioestimator/)
- [`dwm`](./dwm/)
- [`tree`](./tree/)
- [`tree_history`](./tree_history/)

## Estimators

Estimate population totals, ratios, and associated variances.

- [`pe_post_strat`](./pe_post_strat/)
- [`pe_post_strat_ratio`](./pe_post_strat_ratio/)
- [`ve_post_strat`](./ve_post_strat/)
- [`ve_post_strat_ratio`](./ve_post_strat_ratio/)

## Database Facilitation

Connect analyses to FIA databases and explore their contents.

- [`database_mapping`](./database_mapping/)
- [`explore_evals`](./explore_evals/)

## Growth, Removals and Mortality Macros

Build macros for growth, removals, mortality, and related change components.

- [`grm_accretion`](./grm_accretion/)
- [`grm_diversion`](./grm_diversion/)
- [`grm_gross_growth`](./grm_gross_growth/)
- [`grm_gross_ingrowth`](./grm_gross_ingrowth/)
- [`grm_growth_cut`](./grm_growth_cut/)
- [`grm_growth_diversion`](./grm_growth_diversion/)
- [`grm_growth_ingrowth`](./grm_growth_ingrowth/)
- [`grm_growth_mortality`](./grm_growth_mortality/)
- [`grm_growth_reversion`](./grm_growth_reversion/)
- [`grm_growth_survivor`](./grm_growth_survivor/)
- [`grm_harvest_removal`](./grm_harvest_removal/)
- [`grm_ingrowth`](./grm_ingrowth/)
- [`grm_mortality`](./grm_mortality/)
- [`grm_net_change`](./grm_net_change/)
- [`grm_net_growth`](./grm_net_growth/)
- [`grm_removals`](./grm_removals/)
- [`grm_reversion`](./grm_reversion/)
- [`grm_survivor`](./grm_survivor/)
- [`grom_gross_growth`](./grom_gross_growth/)

## Down Woody Material Macros

Build macros for downed woody material components.

- [`dwm_cwd`](./dwm_cwd/)
- [`dwm_duff`](./dwm_duff/)
- [`dwm_fuel`](./dwm_fuel/)
- [`dwm_fwd`](./dwm_fwd/)
- [`dwm_litter`](./dwm_litter/)
- [`dwm_pile`](./dwm_pile/)

## Classes

Core S4 classes used to represent handlers, analyses, mappings, and estimators. These are typically not used directly by uses, and their associated lower-case helpers are used instead.

- [`AnalysisSpec`](./analysisspec-class/)
- [`BaseHandler`](./basehandler-class/)
- [`ChangeAnalysis`](./changeanalysis-class/)
- [`DWMAnalysis`](./dwmanalysis-class/)
- [`DatabaseMapping`](./databasemapping-class/)
- [`Estimator`](./estimator-class/)
- [`EvalHandler`](./evalhandler-class/)
- [`GRMAnalysis`](./grmanalysis-class/)
- [`PostStratifiedRatioEstimator`](./poststratifiedratioestimator-class/)
- [`PostStratifiedRatioVarianceEstimator`](./poststratifiedratiovarianceestimator-class/)
- [`PostStratifiedVarianceEstimator`](./poststratifiedvarianceestimator-class/)
- [`StatusAnalysis`](./statusanalysis-class/)
- [`VarianceEstimator`](./varianceestimator-class/)

