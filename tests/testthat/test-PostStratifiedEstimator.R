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
  expected_cols <- c("COND_STATUS_CD", "var", "estimate", "se")
  expect_true(all(expected_cols %in% colnames(res)))

  # Expect forested (COND_STATUS_CD = 1) estimate
  # Verify that COND_STATUS_CD 1 and 2 are present
  expect_true(1 %in% res$COND_STATUS_CD)
  expect_true(2 %in% res$COND_STATUS_CD)

  # Ensure props sum to 1
  expect_equal(sum(res$estimate), 1)

  # Ensure variance is valid
  expect_true(is.numeric(res$estimate))
  expect_true(is.numeric(res$se))
})

test_that("PostStratifiedEstimator supports mean and total outputs", {
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001) |>
    set_cond_domains(COND_STATUS_CD)

  pe <- PostStratifiedEstimator(handler)

  res_mean <- estimate(pe, cond ~ 1, output = "mean") |>
    dplyr::collect()

  res_total <- estimate(pe, cond ~ 1, output = "total") |>
    dplyr::collect()

  total_area <- handler@tables$pop_estn_unit %>%
    dplyr::summarise(area = sum(AREA_USED, na.rm = TRUE)) %>%
    dplyr::collect() %>%
    dplyr::pull(area)

  joined <- dplyr::inner_join(
    res_mean,
    res_total,
    by = c("COND_STATUS_CD", "var"),
    suffix = c("_mean", "_total")
  )

  expect_equal(joined$estimate_total, joined$estimate_mean * total_area)
  expect_equal(joined$se_total, joined$se_mean * total_area)
})
