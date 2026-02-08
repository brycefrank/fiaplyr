test_that("PostStratifiedRatioEstimator estimates correct ratios", {
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # Create Handlers
  # For Numerator: Group by SPCD
  handler_num <- eval_handler(con, evalid = 1001) |>
    set_tree_domains(SPCD)

  # For Denominator: Group by FORTYPCD
  handler_den <- eval_handler(con, evalid = 1001) |>
    set_cond_domains(FORTYPCD)

  # Create Ratio Estimator
  ratio_est <- PostStratifiedRatioEstimator(handler_num, handler_den)

  # Calculate Ratio: Volume per Area (by Species and Forest Type)
  # Num: tree ~ VOLCFNET
  # Den: cond ~ 1 (Implicit Area)
  res <- estimate_ratio(ratio_est, tree ~ VOLCFNET, cond ~ 1)

  # Verify structure
  expected_cols <- c("SPCD_n", "FORTYPCD_d", "var_n", "var_d", "estimate")
  expect_true(all(expected_cols %in% colnames(res)))

  # Hand computation:
  #
  # Numerator (tree ~ VOLCFNET grouped by SPCD), aggregates = TPA_UNADJ * VOLCFNET:
  #   Plot 101, SPCD=1: 6*10 = 60   |  Plot 101, SPCD=2: 6*15 = 90
  #   Plot 103, SPCD=1: 6*5  = 30   |  Plot 104, SPCD=2: 6*20 = 120
  # Strata means (n_h=2 each):
  #   S1, SPCD=1: 60/2=30           |  S1, SPCD=2: 90/2=45
  #   S2, SPCD=1: 30/2=15           |  S2, SPCD=2: 120/2=60
  # EU means (w_h=0.5 each):
  #   SPCD=1: 0.5*30 + 0.5*15 = 22.5
  #   SPCD=2: 0.5*45 + 0.5*60 = 52.5
  #
  # Denominator (cond ~ 1 grouped by FORTYPCD), aggregates = CONDPROP_UNADJ * ADJ_FACTOR:
  #   Plot 101, F100: 1.0  |  Plot 102, F100: 1.0
  #   Plot 103, F200: 1.0  |  Plot 104, F200: 0.5  |  Plot 104, F300: 0.5
  # Strata means:
  #   S1, F100: 2.0/2=1.0
  #   S2, F200: 1.5/2=0.75  |  S2, F300: 0.5/2=0.25
  # EU means:
  #   F100: 0.5*1.0 = 0.5
  #   F200: 0.5*0.75 = 0.375
  #   F300: 0.5*0.25 = 0.125
  #
  # Ratios (cross product):

  # Sp1 / F100 = 22.5 / 0.5 = 45
  r_sp1_f100 <- res |>
    dplyr::filter(SPCD_n == 1, FORTYPCD_d == 100) |>
    dplyr::pull(estimate)
  expect_equal(r_sp1_f100, 45, tolerance = 0.01)

  # Sp2 / F200 = 52.5 / 0.375 = 140
  r_sp2_f200 <- res |>
    dplyr::filter(SPCD_n == 2, FORTYPCD_d == 200) |>
    dplyr::pull(estimate)
  expect_equal(r_sp2_f200, 140, tolerance = 0.01)

  # Sp1 / F300 = 22.5 / 0.125 = 180
  r_sp1_f300 <- res |>
    dplyr::filter(SPCD_n == 1, FORTYPCD_d == 300) |>
    dplyr::pull(estimate)
  expect_equal(r_sp1_f300, 180, tolerance = 0.01)
})
