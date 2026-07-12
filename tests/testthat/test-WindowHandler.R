test_that("window_handler selects plots without evaluation tables", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  DBI::dbExecute(con, "UPDATE PLOT SET STATECD = CASE WHEN CN <= 104 THEN 25 ELSE 44 END")
  DBI::dbExecute(con, "UPDATE PLOT SET MEASYEAR = CASE WHEN CN <= 104 THEN 2022 ELSE 2026 END")

  window <- spatial_window(states = "MA", counties = 1) &
    temporal_window(2022, type = "measurement")
  handler <- window_handler(con, spec = status_analysis(), window = window)

  expect_s4_class(handler, "WindowHandler")
  expect_true(dplyr::is.tbl(handler@tables$plot))
  expect_null(handler@tables$pop_eval)
  expect_equal(nrow(dplyr::collect(handler@tables$plot)), 4)
  expect_equal(nrow(dplyr::collect(handler@tables$tree)), 4)
  expect_true(is.na(evalid(handler)))
})

test_that("window_handler preserves aggregation and rejects estimation", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- window_handler(
    con,
    window = temporal_window(2020, type = "inventory")
  )

  result <- handler %>%
    aggregate(tree(VOLCFGRS)) %>%
    dplyr::collect()

  expect_true(nrow(result) > 0)
  expect_error(estimate(handler, tree(VOLCFGRS)), "not available for WindowHandler")
})

test_that("spatial_window selects plots intersecting sf polygons", {
  skip_if_not_installed("sf")
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  DBI::dbExecute(con, "ALTER TABLE PLOT ADD COLUMN LON DOUBLE")
  DBI::dbExecute(con, "ALTER TABLE PLOT ADD COLUMN LAT DOUBLE")
  DBI::dbExecute(con, "UPDATE PLOT SET LON = PLOT, LAT = PLOT")

  polygon <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(0, 0, 4.5, 0, 4.5, 4.5, 0, 4.5, 0, 0), ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  handler <- window_handler(con, window = spatial_window(polygon))

  expect_equal(nrow(dplyr::collect(handler@tables$plot)), 4)
})
