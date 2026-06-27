test_that("fiaplyr agrees with FIADB API for a basic volume estimate", {
  skip_if_not_installed("rvalidator")

  con <- DBI::dbConnect(duckdb::duckdb(), fiadb_vt_mini_path())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, 500601) |>
    subset(
      tree(DIA >= 5, STATUSCD == 1, TREECLCD == 2, WOODLAND == 'N'),
      cond(COND_STATUS_CD == 1)
    )

  ests <- handler |>
    PostStratifiedEstimator() |>
    estimate(tree(VOLCFNET), output = "total")

  fiaplyr_total <- ests$estimate[[1]]

  report <- rvalidator::fullreport(
    list(
      snum = 15,
      wc = 502006,
      rselected = "County code and name",
      cselected = "Land Use - Major"
    )
  )

  api_total <- report$totals$ESTIMATE
  rel_diff <- abs(fiaplyr_total - api_total) / api_total

  expect_true(rel_diff < 0.001)
})

test_that("fiaplyr agrees with FIADB API for a GRM mortality total", {
  skip_if_not_installed("rvalidator")

  con <- DBI::dbConnect(duckdb::duckdb(), fiadb_vt_mini_path())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, 501103, spec = grm_analysis())

  ests <- handler |>
    PostStratifiedEstimator() |>
    estimate(
      tree_history(grm_ingrowth(annualize = TRUE)),
      output = "total"
    )

  fiaplyr_total <- ests$estimate[[1]]

  report <- rvalidator::fullreport(
    list(
      snum = 574157,
      wc = 502011,
      rselected = "Species",
      cselected = "Land Use - Major"
    )
  )

  api_total <- report$totals$ESTIMATE[[1]]
  rel_diff <- abs(fiaplyr_total - api_total) / api_total

  expect_true(rel_diff < 0.001)
})

test_that("fiaplyr agrees with FIADB API for a GRM ingrowth total", {
  skip_if_not_installed("rvalidator")

  con <- DBI::dbConnect(duckdb::duckdb(), fiadb_vt_mini_path())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, 501103, spec = grm_analysis())

  ests <- handler |>
    PostStratifiedEstimator() |>
    estimate(
      tree_history(grm_ingrowth(1, annualize = TRUE)),
      output = "total"
    )

  fiaplyr_total <- ests$estimate[[1]]

  report <- rvalidator::fullreport(
    list(
      snum = 3000,
      wc = 502011,
      rselected = "Species",
      cselected = "Land Use - Major"
    )
  )

  api_total <- report$totals$ESTIMATE[[1]]
  rel_diff <- abs(fiaplyr_total - api_total) / api_total

  expect_true(rel_diff < 0.001)
})