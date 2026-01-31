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
  expect_true(all(c("SPCD", "FORTYPCD", "estimate") %in% colnames(res)))

  # Verify values (See scratchpad in thought process for derivation)
  # Sp1 / F100
  r_sp1_f100 <- res |>
    dplyr::filter(SPCD == 1, FORTYPCD == 100) |>
    dplyr::pull(estimate)
  expect_equal(r_sp1_f100, 45, tolerance = 0.01)

  # Sp2 / F200
  r_sp2_f200 <- res |>
    dplyr::filter(SPCD == 2, FORTYPCD == 200) |>
    dplyr::pull(estimate)
  expect_equal(r_sp2_f200, 140, tolerance = 0.01)

  # Check Cross Product size
  # Species: 1, 2. Fortyp: 100, 200, 300.
  # 2 * 3 = 6 rows expected (assuming all combinations are returned, even if Denom is 0?)
  # If Denom is 0, Ratio is Inf or NaN.
  # F300 Area = 125.
  # Sp1 / F300 = 22500 / 125 = 180.
  r_sp1_f300 <- res |>
    dplyr::filter(SPCD == 1, FORTYPCD == 300) |>
    dplyr::pull(estimate)
  expect_equal(r_sp1_f300, 180, tolerance = 0.01)
})
