test_that("PostStratifiedEstimator estimates tree ~ VOLCFGRS | 1 correctly", {
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # Update TREE table to include TPA_UNADJ (required for estimation)
  tree_df <- DBI::dbReadTable(con, "TREE")
  tree_df$TPA_UNADJ <- 1.0
  DBI::dbWriteTable(con, "TREE", tree_df, overwrite = TRUE)

  # Also need weights in POP_STRATUM or similar?
  # PostStratifiedEstimator calculates w_h = P1POINTCNT / P1PNTCNT_EU
  # helper-data.R has POP_STRATUM and POP_ESTN_UNIT but columns might be missing.

  # POP_ESTN_UNIT needs P1PNTCNT_EU
  # POP_STRATUM needs P1POINTCNT, P2POINTCNT, AREA_USED

  # Let's inspect helper-data.R again for these columns.

  # helper-data.R:
  # POP_ESTN_UNIT: CN, EVAL_CN. Missing P1PNTCNT_EU.
  # POP_STRATUM: CN, ESTN_UNIT_CN. Missing P1POINTCNT, P2POINTCNT, AREA_USED.

  # I need to populate these for PostStratifiedEstimator to work.

  # Update POP_ESTN_UNIT
  eu_df <- DBI::dbReadTable(con, "POP_ESTN_UNIT")
  eu_df$P1PNTCNT_EU <- 100
  DBI::dbWriteTable(con, "POP_ESTN_UNIT", eu_df, overwrite = TRUE)

  # Update POP_STRATUM
  strat_df <- DBI::dbReadTable(con, "POP_STRATUM")
  strat_df$P1POINTCNT <- 50 # 2 strata, say 50 each
  strat_df$P2POINTCNT <- 2
  strat_df$AREA_USED <- 1000
  DBI::dbWriteTable(con, "POP_STRATUM", strat_df, overwrite = TRUE)

  # Setup handler
  handler <- eval_handler(con, evalid = 1001)

  # Create Estimator
  estimator <- PostStratifiedEstimator(handler)

  # Run estimate with '1' in the formula
  res <- estimate(estimator, tree ~ VOLCFGRS | 1)

  # Check structure
  expect_true(is.data.frame(res))
  expect_true("VOLCFGRS" %in% names(res))
  expect_true("1" %in% names(res))

  # Check values
  # With TPA_UNADJ = 1, "1" column should represent estimated tree count.
  # We have 4 trees in helper-data.
  # PLT_CN 101 (Strat 1): 2 trees
  # PLT_CN 103 (Strat 2): 1 tree
  # PLT_CN 104 (Strat 2): 1 tree
  # PLT 102 (Strat 1) has no trees (implicit 0)

  # Strata 1: 2 plots (101, 102). 101 has 2 trees. 102 has 0. Mean = 1 tree/plot.
  # Strata 2: 2 plots (103, 104). 103 has 1 tree. 104 has 1 tree. Mean = 1 tree/plot.

  # Weights: w_h = P1POINTCNT / P1PNTCNT_EU = 50 / 100 = 0.5 for both.
  # But typically w_h sum to 1? Here 0.5 + 0.5 = 1.

  # Estimate = sum(w_h * mean_h)
  # Est = 0.5 * 1 + 0.5 * 1 = 1 tree per plot?

  # Wait, .estimate_tree_strata_internal calculates mean per stratum (sum / n_h).
  # n_h is P2POINTCNT in POP_STRATUM?
  # No, n_h is usually derived from POP_STRATUM or calculated.
  # In PostStratifiedEstimator:
  # strata_weights has P2POINTCNT.

  # .estimate_tree_strata_internal:
  # summarise( ... / n_h )
  # n_h comes from `combined_data`.
  # `combined_data` joins `strata_summary`.
  # `.get_strata_summary` (in utils.R probably) likely calculates n_h from plots.

  # If n_h is number of plots in stratum.
  # Strat 1: 2 plots.
  # Strat 2: 2 plots.

  # Strat 1 sum(TPA) = 2 (from plot 101) + 0 = 2. Mean = 2/2 = 1.
  # Strat 2 sum(TPA) = 1 (103) + 1 (104) = 2. Mean = 2/2 = 1.

  # EU Estimate = sum(w_h * mean_h) = 0.5 * 1 + 0.5 * 1 = 1.

  # Total Estimate (Estimate column usually implies per-acre or total?
  # The code sums: `sum(.x * w_eu, na.rm = TRUE)` where `.x` is `sum(w_h * .x)` (from eu_internal).
  # So result is effectively mean per plot/unit.

  # If the estimators returns totals, it usually multiplies by area.
  # But let's just check valid numbers are returned.

  expect_equal(res$`1`[1], 1)
})
