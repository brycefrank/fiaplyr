test_that("PostStratifiedRatioEstimator estimates correct ratios", {
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # Create Handlers
  # For Numerator: Group by SPCD
  handler_num <- eval_handler(con, evalid = 1001) |>
    partition(tree(SPCD))

  # For Denominator: Group by FORTYPCD
  handler_den <- eval_handler(con, evalid = 1001) |>
    partition(cond(FORTYPCD))

  # Create Ratio Estimator
  ratio_est <- PostStratifiedRatioEstimator(handler_num, handler_den)

  # Calculate Ratio: Volume per Area (by Species and Forest Type)
  # Num: tree(VOLCFNET)
  # Den: cond() (Implicit Area)
  res <- estimate_ratio(ratio_est, tree(VOLCFNET), cond())

  # Verify structure
  expected_cols <- c("SPCD_n", "FORTYPCD_d", "var_n", "var_d", "estimate", "se")
  expect_true(all(expected_cols %in% colnames(res)))
  expect_true(all(is.finite(res$se)))
  expect_true(all(res$se >= 0))

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

test_that("PostStratifiedRatioEstimator supports ratios without explicit domains", {
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001)
  ratio_est <- PostStratifiedRatioEstimator(handler, handler)

  res <- estimate_ratio(ratio_est, tree(VOLCFNET), cond()) |>
    dplyr::collect()

  expect_identical(colnames(res), c("var_n", "var_d", "estimate", "se"))
  expect_equal(nrow(res), 1)
  expect_equal(res$var_n, "VOLCFNET")
  expect_equal(res$var_d, "prop")
  expect_true(is.finite(res$estimate))
  expect_true(is.finite(res$se))
  expect_true(res$se >= 0)
})

test_that("PostStratifiedRatioEstimator supports shared domains on both sides", {
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001) |>
    partition(tree(SPCD))

  ratio_est <- PostStratifiedRatioEstimator(handler, handler)

  res <- estimate_ratio(ratio_est, tree(VOLCFNET), tree(DIA)) |>
    dplyr::collect()

  expect_identical(colnames(res), c("SPCD_n", "SPCD_d", "var_n", "var_d", "estimate", "se"))
  expect_equal(nrow(res), 4)
  expect_true(all(c(1, 2) %in% res$SPCD_n))
  expect_true(all(c(1, 2) %in% res$SPCD_d))
  expect_true(all(is.finite(res$estimate)))
  expect_true(all(is.finite(res$se)))
  expect_true(all(res$se >= 0))
})

test_that("PostStratifiedRatioEstimator can restrict ratios to matched domains", {
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001) |>
    partition(tree(SPCD))

  ratio_est <- PostStratifiedRatioEstimator(handler, handler)

  res <- estimate_ratio(
    ratio_est,
    tree(VOLCFNET),
    tree(DIA),
    domain_pairing = "matched"
  ) |>
    dplyr::collect()

  expect_identical(colnames(res), c("SPCD_n", "SPCD_d", "var_n", "var_d", "estimate", "se"))
  expect_equal(nrow(res), 2)
  expect_equal(res$SPCD_n, res$SPCD_d)
  expect_true(all(c(1, 2) %in% res$SPCD_n))
  expect_true(all(is.finite(res$estimate)))
  expect_true(all(is.finite(res$se)))
  expect_true(all(res$se >= 0))
})

test_that("PostStratifiedRatioEstimator rejects matched domains with incompatible schemas", {
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler_num <- eval_handler(con, evalid = 1001) |>
    partition(tree(SPCD))

  handler_den <- eval_handler(con, evalid = 1001) |>
    partition(cond(FORTYPCD))

  ratio_est <- PostStratifiedRatioEstimator(handler_num, handler_den)

  expect_error(
    estimate_ratio(
      ratio_est,
      tree(VOLCFNET),
      cond(),
      domain_pairing = "matched"
    ),
    "requires numerator and denominator to have the same domain columns"
  )
})

test_that("PostStratifiedRatioEstimator can append numerator and denominator component stats", {
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001)
  ratio_est <- PostStratifiedRatioEstimator(handler, handler)

  res_default <- estimate_ratio(ratio_est, tree(VOLCFNET), cond()) |>
    dplyr::collect()
  expect_false(any(c("estimate_n", "se_n", "estimate_d", "se_d") %in% colnames(res_default)))

  res_components <- estimate_ratio(
    ratio_est,
    tree(VOLCFNET),
    cond(),
    include_components = TRUE
  ) |>
    dplyr::collect()

  expect_true(all(c("estimate_n", "se_n", "estimate_d", "se_d") %in% colnames(res_components)))
  expect_true(all(is.finite(res_components$estimate_n)))
  expect_true(all(is.finite(res_components$se_n)))
  expect_true(all(is.finite(res_components$estimate_d)))
  expect_true(all(is.finite(res_components$se_d)))
  expect_equal(res_components$estimate, res_components$estimate_n / res_components$estimate_d)
})
