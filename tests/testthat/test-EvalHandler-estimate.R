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
