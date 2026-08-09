test_that("estimator specifications compose through S4 dispatch", {
  variance_estimator <- ve_post_strat()
  point_estimator <- pe_post_strat(var_est = variance_estimator)

  expect_s4_class(variance_estimator, "PostStratifiedVarianceEstimator")
  expect_s4_class(variance_estimator, "VarianceEstimator")
  expect_s4_class(point_estimator, "PostStratifiedEstimator")
  expect_identical(point_estimator@var_est, variance_estimator)
  expect_null(point_estimator@handler)

  method <- methods::selectMethod(
    ".estimate_composed",
    c("PostStratifiedEstimator", "PostStratifiedVarianceEstimator")
  )
  expect_s4_class(method, "MethodDefinition")
})

test_that("EvalHandler estimate() accepts an explicit estimator", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001)

  default <- estimate(handler, tree(VOLCFNET)) |>
    dplyr::collect()
  explicit <- estimate(
    handler,
    tree(VOLCFNET),
    estimator = pe_post_strat(var_est = ve_post_strat())
  ) |>
    dplyr::collect()

  expect_equal(explicit, default)
})

test_that("explicit ratio estimator supports ratio targets", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001) |>
    partition(tree(SPCD))

  result <- estimate(
    handler,
    ratio(tree(VOLCFNET), cond()),
    estimator = pe_post_strat_ratio(var_est = ve_post_strat_ratio()),
    include_components = TRUE
  ) |>
    dplyr::collect()

  expect_true(all(c(
    "estimate", "se", "estimate_n", "se_n", "estimate_d", "se_d"
  ) %in% colnames(result)))
})

test_that("non-ratio estimator rejects ratio targets", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001)

  expect_error(
    estimate(
      handler,
      ratio(tree(VOLCFNET), cond()),
      estimator = pe_post_strat(var_est = ve_post_strat())
    ),
    "requires a ratio point estimator"
  )
})

test_that("EvalHandler estimate() forwards ratio-specific options", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001) |>
    partition(tree(SPCD))

  res <- estimate(
    handler,
    ratio(tree(VOLCFNET), tree(DIA)),
    domain_pairing = "matched",
    include_components = TRUE
  ) |>
    dplyr::collect()

  expect_true(all(c("estimate_n", "se_n", "estimate_d", "se_d") %in% colnames(res)))
  expect_equal(res$SPCD_n, res$SPCD_d)
})

test_that("EvalHandler estimate() forwards ratio denominator partition overrides", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001) |>
    partition(tree(SPCD))

  res <- estimate(
    handler,
    ratio(
      tree(VOLCFNET),
      cond(),
      den_partitions = list(cond(FORTYPCD))
    )
  ) |>
    dplyr::collect()

  expect_true(all(c("SPCD_n", "FORTYPCD_d", "estimate", "se") %in% colnames(res)))
  expect_true(nrow(res) > 0)
})

test_that("EvalHandler estimate() rejects output and margins for ratio() helper", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001)

  expect_error(
    estimate(handler, ratio(tree(VOLCFNET), cond()), output = "total"),
    "not supported with `ratio"
  )

  expect_error(
    estimate(handler, ratio(tree(VOLCFNET), cond()), margins = TRUE),
    "not supported with `ratio"
  )
})

test_that("estimate() accepts multiple scopes", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001)

  combined <- estimate(handler, cond(), tree(VOLCFGRS)) |>
    dplyr::collect()

  expect_setequal(combined$var, c("prop", "VOLCFGRS"))

  cond_est <- estimate(handler, cond()) |>
    dplyr::collect()
  tree_est <- estimate(handler, tree(VOLCFGRS)) |>
    dplyr::collect()

  expect_equal(
    combined |>
      dplyr::filter(var == "prop") |>
      dplyr::select(estimate, se),
    cond_est |>
      dplyr::select(estimate, se)
  )
  expect_equal(
    combined |>
      dplyr::filter(var == "VOLCFGRS") |>
      dplyr::select(estimate, se),
    tree_est |>
      dplyr::select(estimate, se)
  )
})

test_that("estimate() accepts multiple same-scope targets in one call", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001)

  res <- estimate(handler, tree(VOLCFGRS), tree(VOLCFNET)) |>
    dplyr::collect()

  expect_setequal(res$var, c("VOLCFGRS", "VOLCFNET"))
})

test_that("estimate() supports margins and totals across scopes", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001)

  margins_res <- estimate(handler, cond(), tree(VOLCFGRS), margins = TRUE) |>
    dplyr::collect()
  expect_true(all(c("var", "estimate", "se", "is_marginal") %in% colnames(margins_res)))
  expect_setequal(margins_res$var, c("prop", "VOLCFGRS"))

  total_res <- estimate(handler, cond(), tree(VOLCFGRS), output = "total") |>
    dplyr::collect()
  expect_setequal(total_res$var, c("prop", "VOLCFGRS"))
})

test_that("estimate() rejects mixing ratio() with point targets", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001)

  expect_error(
    estimate(handler, ratio(tree(VOLCFNET), cond()), tree(BA)),
    "cannot be combined"
  )
  expect_error(
    estimate(handler, tree(BA), ratio(tree(VOLCFNET), cond())),
    "cannot be combined"
  )
})
