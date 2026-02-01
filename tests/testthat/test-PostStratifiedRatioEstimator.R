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
  # Note: The estimator now suffixes domain variables to avoid collisions
  # and includes variable metadata columns.
  expected_cols <- c(
    "SPCD_n", "FORTYPCD_d", "estimate",
    "var_n", "var_d", "n_observed", "n_missing"
  )
  expect_true(all(expected_cols %in% colnames(res)))

  # Verify values
  # Sp1 / F100
  r_sp1_f100 <- res |>
    dplyr::filter(SPCD_n == 1, FORTYPCD_d == 100) |>
    dplyr::pull(estimate)
  expect_equal(r_sp1_f100, 45, tolerance = 0.01)

  # Sp2 / F200
  r_sp2_f200 <- res |>
    dplyr::filter(SPCD_n == 2, FORTYPCD_d == 200) |>
    dplyr::pull(estimate)
  expect_equal(r_sp2_f200, 140, tolerance = 0.01)

  # Check Cross Product size
  # Species: 1, 2. Fortyp: 100, 200, 300.
  # F300 Area = 125.
  # Sp1 / F300 = 22500 / 125 = 180.
  r_sp1_f300 <- res |>
    dplyr::filter(SPCD_n == 1, FORTYPCD_d == 300) |>
    dplyr::pull(estimate)
  expect_equal(r_sp1_f300, 180, tolerance = 0.01)
})
