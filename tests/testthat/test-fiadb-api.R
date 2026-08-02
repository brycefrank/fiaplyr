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

  fiaplyr_total <- dplyr::pull(ests, estimate)[[1]]

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

  expect_true(rel_diff < 0.001, info = sprintf("rel_diff = %.10f (threshold = 0.001)", rel_diff))
})

test_that("fiaplyr agrees with FIADB API for a GRM mortality total", {
  skip_if_not_installed("rvalidator")

  con <- DBI::dbConnect(duckdb::duckdb(), fiadb_vt_mini_path())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, 501103, spec = grm_analysis())

  ests <- handler |>
    PostStratifiedEstimator() |>
    estimate(
      tree_history(grm_mortality(VOLCFSND, annualize = TRUE)),
      output = "total"
    )

  fiaplyr_total <- dplyr::pull(ests, estimate)[[1]]

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

  expect_true(rel_diff < 0.001, info = sprintf("rel_diff = %.10f (threshold = 0.001)", rel_diff))
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

  fiaplyr_total <- dplyr::pull(ests, estimate)[[1]]

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

  expect_true(rel_diff < 0.001, info = sprintf("rel_diff = %.10f (threshold = 0.001)", rel_diff))
})

test_that("fiaplyr agrees with FIADB API for GRM annual removals of sound bole volume", {
  skip_if_not_installed("rvalidator")

  con <- DBI::dbConnect(duckdb::duckdb(), fiadb_vt_mini_path())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, 501103, spec = grm_analysis())

  ests <- handler |>
    PostStratifiedEstimator() |>
    estimate(
      tree_history(grm_harvest_removal(VOLCFSND, annualize = TRUE)),
      output = "total"
    )

  fiaplyr_total <- dplyr::pull(ests, estimate)[[1]]

  report <- rvalidator::fullreport(
    list(
      snum = 574161,
      wc = 502011,
      rselected = "Species",
      cselected = "Land Use - Major"
    )
  )

  api_total <- report$totals$ESTIMATE[[1]]
  rel_diff <- abs(fiaplyr_total - api_total) / api_total

  expect_true(rel_diff < 0.001, info = sprintf("rel_diff = %.10f (threshold = 0.001)", rel_diff))
})

test_that("fiaplyr agrees with FIADB API for GRM annual reversion stem density", {
  skip_if_not_installed("rvalidator")

  con <- DBI::dbConnect(duckdb::duckdb(), fiadb_vt_mini_path())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, 501103, spec = grm_analysis())

  ests <- handler |>
    PostStratifiedEstimator() |>
    estimate(
      tree_history(grm_reversion(1, annualize = TRUE)),
      output = "total"
    )

  fiaplyr_total <- dplyr::pull(ests, estimate)[[1]]

  report <- rvalidator::fullreport(
    list(
      snum = 3012,
      wc = 502011,
      rselected = "Species",
      cselected = "Land Use - Major"
    )
  )

  api_total <- report$totals$ESTIMATE[[1]]
  rel_diff <- abs(fiaplyr_total - api_total) / api_total

  expect_true(rel_diff < 0.001, info = sprintf("rel_diff = %.10f (threshold = 0.001)", rel_diff))
})

.lookup_snum_from_parameters <- function(metric_phrase) {
  defs <- get(".get_rvalidator_parameters", mode = "function")("snum")

  matches <- defs[
    grepl(metric_phrase, defs$ATTRIBUTE_DESCR, ignore.case = TRUE) &
      defs$LAND_BASIS == "Forest land" &
      defs$ESTN_UNITS_DISPLAY == "cubic feet",
    ,
    drop = FALSE
  ]

  snums <- unique(matches$ATTRIBUTE_NBR)
  if (length(snums) == 0) {
    stop("Unable to find snum for metric phrase: ", metric_phrase, call. = FALSE)
  }

  as.integer(snums[[1]])
}

.run_grm_api_total <- function(target_macro, snum) {
  con <- DBI::dbConnect(duckdb::duckdb(), fiadb_vt_mini_path())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, 501103, spec = grm_analysis())

  ests <- handler |>
    PostStratifiedEstimator() |>
    estimate(tree_history(!!target_macro), output = "total")

  fiaplyr_total <- dplyr::pull(ests, estimate)[[1]]

  report <- rvalidator::fullreport(
    list(
      snum = snum,
      wc = 502011,
      rselected = "Species",
      cselected = "Land Use - Major"
    )
  )

  api_total <- report$totals$ESTIMATE[[1]]
  abs(fiaplyr_total - api_total) / api_total
}

test_that("fiaplyr agrees with FIADB API for a GRM gross growth total", {
  skip("This test is skipped due to a known discrepancy in handling missing original tree records.")

  snum <- 574165L
  rel_diff <- .run_grm_api_total(grm_gross_growth(VOLCFSND, annualize = TRUE), snum)

  expect_true(rel_diff < 0.001, info = sprintf("rel_diff = %.10f (threshold = 0.001)", rel_diff))
})

test_that("fiaplyr agrees with FIADB API for a GRM net growth total", {
  skip("This test is skipped due to a known discrepancy in handling missing original tree records.")

  snum <- 574155L
  rel_diff <- .run_grm_api_total(grm_net_growth(VOLCFSND, annualize = TRUE), snum)

  expect_true(rel_diff < 0.001, info = sprintf("rel_diff = %.10f (threshold = 0.001)", rel_diff))
})

test_that("fiaplyr agrees with FIADB API for a GRM net change total", {
  skip_if_not_installed("rvalidator")

  snum <- 574167L
  rel_diff <- .run_grm_api_total(grm_net_change(VOLCFSND, annualize = TRUE), snum)

  expect_true(rel_diff < 0.001, info = sprintf("rel_diff = %.10f (threshold = 0.001)", rel_diff))
})

test_that("fiaplyr agrees with FIADB API for a DWM CWD carbon total", {
  skip_if_not_installed("rvalidator")

  con <- DBI::dbConnect(duckdb::duckdb(), fiadb_vt_mini_path())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, 501007, spec = dwm_analysis())

  ests <- handler |>
    PostStratifiedEstimator() |>
    estimate(dwm(dwm_cwd(CARBON)), output = "total")

  fiaplyr_total <- dplyr::pull(ests, estimate)[[1]]

  report <- rvalidator::fullreport(
    list(
      snum = 116,
      wc = 502010,
      rselected = "All live stocking",
      cselected = "All live stocking"
    )
  )

  api_total <- report$totals$ESTIMATE[[1]]
  rel_diff <- abs(fiaplyr_total - api_total) / api_total

  expect_true(rel_diff < 0.001, info = sprintf("rel_diff = %.10f (threshold = 0.001)", rel_diff))
})

test_that("fiaplyr agrees with FIADB API for DWM component totals", {
  skip_if_not_installed("rvalidator")

  cases <- list(
    list(snum = 114, target = quote(dwm_cwd(VOLCF)), label = "CWD volume"),
    list(snum = 113, target = quote(dwm_cwd(LPA)), label = "CWD pieces"),
    list(snum = 120, target = quote(dwm_fwd(VOLCF, size = "ALL")), label = "FWD volume (all sizes)"),
    list(snum = 105, target = quote(dwm_fwd(DRYBIO, size = "SM")), label = "FWD biomass (small)"),
    list(snum = 112, target = quote(dwm_fwd(CARBON, size = "LG")), label = "FWD carbon (large)")
  )

  con <- DBI::dbConnect(duckdb::duckdb(), fiadb_vt_mini_path())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  for (case in cases) {
    handler <- eval_handler(con, 501007, spec = dwm_analysis())

    ests <- handler |>
      PostStratifiedEstimator() |>
      estimate(dwm(!!case$target), output = "total")

    fiaplyr_total <- dplyr::pull(ests, estimate)[[1]]

    report <- rvalidator::fullreport(
      list(
        snum = case$snum,
        wc = 502010,
        rselected = "All live stocking",
        cselected = "All live stocking"
      )
    )

    api_total <- report$totals$ESTIMATE[[1]]
    rel_diff <- abs(fiaplyr_total - api_total) / api_total

    expect_true(
      rel_diff < 0.001,
      info = sprintf(
        "%s (snum %s): rel_diff = %.10f (threshold = 0.001)",
        case$label, case$snum, rel_diff
      )
    )
  }
})
