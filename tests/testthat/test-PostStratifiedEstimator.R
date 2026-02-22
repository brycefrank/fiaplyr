test_that("PostStratifiedEstimator estimates correct forested area", {
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # Create Handlers
  # For cond: Group by COND_STATUS_CD
  handler <- eval_handler(con, evalid = 1001) |>
    set_cond_domains(COND_STATUS_CD)

  # Create Estimator
  pe <- PostStratifiedEstimator(handler)

  # Estimate area for COND_STATUS_CD
  res <- estimate(pe, cond ~ 1) |>
    dplyr::collect()

  # Verify structure
  expected_cols <- c("COND_STATUS_CD", "prop", "prop_se")
  expect_true(all(expected_cols %in% colnames(res)))

  # Expect forested (COND_STATUS_CD = 1) estimate
  # Verify that COND_STATUS_CD 1 and 2 are present
  expect_true(1 %in% res$COND_STATUS_CD)
  expect_true(2 %in% res$COND_STATUS_CD)

  # Ensure props sum to 1
  expect_equal(sum(res$prop), 1)

  # Ensure variance is valid
  expect_true(is.numeric(res$prop))
  expect_true(is.numeric(res$prop_se))
})
