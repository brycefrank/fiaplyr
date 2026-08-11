test_that("window_handler() returns a WindowHandler with all plots by default", {
  con <- setup_status_test_db()
  wh <- window_handler(con)

  expect_s4_class(wh, "WindowHandler")
  expect_equal(
    wh@tables$plot %>% dplyr::tally() %>% dplyr::collect() %>% dplyr::pull(n),
    8
  )
})

test_that("window_handler() defaults to the status analysis spec", {
  con <- setup_status_test_db()
  wh <- window_handler(con)

  expect_s4_class(wh@spec, "StatusAnalysis")
  expect_s4_class(wh@spec, "AnalysisSpec")
})

test_that("window_handler() composes with the status spec for aggregation", {
  con <- setup_status_test_db()

  wh <- window_handler(con)

  tree_res <- wh %>%
    aggregate(tree(VOLCFGRS)) %>%
    dplyr::collect()
  expect_true("VOLCFGRS" %in% colnames(tree_res))
  expect_equal(nrow(tree_res), 8)

  cond_res <- wh %>%
    partition(cond(COND_STATUS_CD)) %>%
    aggregate(cond()) %>%
    dplyr::collect()
  expect_true(all(c("COND_STATUS_CD", "prop") %in% colnames(cond_res)))
})

test_that("window_handler() verbs compose with aggregation", {
  con <- setup_status_test_db()

  res <- window_handler(con) %>%
    transform(tree(BA = 0.005454 * DIA^2)) %>%
    subset(tree(SPCD == 1)) %>%
    partition(tree(SPCD)) %>%
    aggregate(tree(BA)) %>%
    dplyr::collect()

  expect_true(all(c("SPCD", "BA") %in% colnames(res)))
  expect_true(all(res$SPCD == 1))
})

test_that("window_handler() supports augment() and materialize()", {
  con <- setup_status_test_db()

  species_ref <- data.frame(
    SPCD = c(1, 2),
    COMMON_NAME = c("Pine", "Oak"),
    stringsAsFactors = FALSE
  )

  augmented <- window_handler(con) %>%
    augment(tree(species_ref, by = "SPCD"))

  expect_length(augmented@pipeline$tree$augment, 1)

  res <- suppressWarnings(
    materialize(augmented, "tree") %>% dplyr::collect()
  )
  expect_true("COMMON_NAME" %in% names(res))
  expect_setequal(unique(res$COMMON_NAME), c("Pine", "Oak"))

  agg <- suppressWarnings(
    augmented %>%
      partition(tree(COMMON_NAME)) %>%
      aggregate(tree(VOLCFGRS)) %>%
      dplyr::collect()
  )
  expect_true("COMMON_NAME" %in% names(agg))
})

test_that("window_handler() composes with the GRM spec", {
  con <- setup_grm_test_db()

  wh <- window_handler(con, spec = grm_analysis())

  expect_s4_class(wh@spec, "GRMAnalysis")
  expect_false(is.null(wh@tables$tree_history))

  res <- wh %>%
    aggregate(tree_history(mortality = grm_mortality())) %>%
    dplyr::collect()
  expect_true(nrow(res) > 0)
  expect_true("mortality" %in% colnames(res))
})

test_that("window_handler() rejects the DWM spec with an explanatory error", {
  con <- setup_dwm_test_db()

  expect_error(
    window_handler(con, spec = dwm_analysis()),
    "requires an evaluation context"
  )
})

test_that("window_handler() filters by statecd", {
  con <- setup_status_test_db()

  DBI::dbExecute(con, "UPDATE PLOT SET STATECD = 9 WHERE CN = 101")
  wh <- window_handler(con, statecd = 1)

  n <- wh@tables$plot %>% dplyr::tally() %>% dplyr::collect() %>% dplyr::pull(n)
  expect_equal(n, 7)
})

test_that("window_handler() filters by invyrs", {
  con <- setup_status_test_db()

  DBI::dbExecute(con, "UPDATE PLOT SET INVYR = 2010 WHERE CN = 101")
  wh <- window_handler(con, invyrs = 2010)

  invyrs <- wh@tables$plot %>%
    dplyr::distinct(INVYR) %>%
    dplyr::collect() %>%
    dplyr::pull(INVYR)
  expect_setequal(invyrs, 2010)
})

test_that("window_handler() filters by county table", {
  con <- setup_status_test_db()

  DBI::dbExecute(con, "UPDATE PLOT SET COUNTYCD = 5 WHERE CN = 101")
  counties <- data.frame(STATECD = 1, COUNTYCD = 5)
  wh <- window_handler(con, county = counties)

  n <- wh@tables$plot %>% dplyr::tally() %>% dplyr::collect() %>% dplyr::pull(n)
  expect_equal(n, 1)
})

test_that("window_handler() filters by single statecd + countycd", {
  con <- setup_status_test_db()

  DBI::dbExecute(con, "UPDATE PLOT SET COUNTYCD = 5 WHERE CN = 101")
  wh <- window_handler(con, statecd = 1, countycd = 5)

  n <- wh@tables$plot %>% dplyr::tally() %>% dplyr::collect() %>% dplyr::pull(n)
  expect_equal(n, 1)
})

test_that("window_handler() rejects countycd without statecd", {
  con <- setup_status_test_db()
  expect_error(
    window_handler(con, countycd = 5),
    "`countycd` is ambiguous without `statecd`"
  )
})

test_that("window_handler() rejects countycd across multiple states", {
  con <- setup_status_test_db()
  expect_error(
    window_handler(con, statecd = c(1, 9), countycd = 5),
    "Use the `county` table"
  )
})

test_that("window_handler() rejects county and countycd together", {
  con <- setup_status_test_db()
  counties <- data.frame(STATECD = 1, COUNTYCD = 5)
  expect_error(
    window_handler(con, county = counties, countycd = 5),
    "Provide either `county` or `countycd`"
  )
})

test_that("window_handler() rejects malformed bbox and county tables", {
  con <- setup_status_test_db()
  expect_error(window_handler(con, bbox = c(-73, 43)), "length 4")
  expect_error(
    window_handler(con, county = data.frame(STATE = 1)),
    "STATECD.*COUNTYCD"
  )
})

test_that("window_handler() filters by bbox using LON/LAT", {
  con <- setup_status_test_db()

  DBI::dbExecute(con, "ALTER TABLE PLOT ADD COLUMN LON DOUBLE")
  DBI::dbExecute(con, "ALTER TABLE PLOT ADD COLUMN LAT DOUBLE")
  DBI::dbExecute(con, "UPDATE PLOT SET LON = -72.5, LAT = 43.0 WHERE CN < 200")
  DBI::dbExecute(con, "UPDATE PLOT SET LON = -75.0, LAT = 40.0 WHERE CN >= 200")

  wh <- window_handler(con, bbox = c(-73, 42, -72, 44))

  n <- wh@tables$plot %>% dplyr::tally() %>% dplyr::collect() %>% dplyr::pull(n)
  expect_equal(n, 4)
})

test_that("window_handler() intersects plots with a geometry", {
  skip_if_not_installed("sf")
  con <- DBI::dbConnect(duckdb::duckdb(), fiadb_vt_mini_path(), read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  win <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(-73.0, 43.0), c(-73.0, 44.0), c(-72.0, 44.0),
        c(-72.0, 43.0), c(-73.0, 43.0)
      ))),
      crs = 4269
    )
  )

  wh <- window_handler(con, geometry = win)

  coords <- wh |> coordinates()
  expect_true(nrow(coords) > 0)
  expect_true(all(coords$LON >= -73.0 & coords$LON <= -72.0))
  expect_true(all(coords$LAT >= 43.0 & coords$LAT <= 44.0))
})

test_that("window_handler() intersects in a projected CRS", {
  skip_if_not_installed("sf")
  con <- DBI::dbConnect(duckdb::duckdb(), fiadb_vt_mini_path(), read_only = TRUE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  win_4269 <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(-73.0, 43.0), c(-73.0, 44.0), c(-72.0, 44.0),
        c(-72.0, 43.0), c(-73.0, 43.0)
      ))),
      crs = 4269
    )
  )
  win <- sf::st_transform(win_4269, crs = 26918)

  wh <- window_handler(con, geometry = win)
  coords <- wh |> coordinates(as_sf = TRUE)

  expect_true(all(sf::st_is_valid(sf::st_as_sf(coords))))
})

test_that("window_handler() does not support estimate()", {
  con <- setup_status_test_db()
  wh <- window_handler(con)

  expect_error(
    wh |> estimate(tree(VOLCFGRS)),
    "estimation is not yet supported for `WindowHandler`"
  )
})
