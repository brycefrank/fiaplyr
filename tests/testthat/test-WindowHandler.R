test_that("window_handler() returns a WindowHandler with all plots by default", {
  con <- setup_status_test_db()
  wh <- window_handler(con)

  expect_s4_class(wh, "WindowHandler")
  expect_equal(
    wh@tables$plot %>% dplyr::tally() %>% dplyr::collect() %>% dplyr::pull(n),
    8
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
