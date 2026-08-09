add_coord_cols <- function(con, lon = "LON", lat = "LAT") {
  DBI::dbExecute(con, sprintf("ALTER TABLE PLOT ADD COLUMN %s DOUBLE", lon))
  DBI::dbExecute(con, sprintf("ALTER TABLE PLOT ADD COLUMN %s DOUBLE", lat))
}

test_that("coordinates() returns plot identifiers and LON/LAT by default", {
  con <- setup_status_test_db()

  add_coord_cols(con)
  DBI::dbExecute(con, "UPDATE PLOT SET LON = -72.5, LAT = 43.0")

  handler <- eval_handler(con, evalid = 1001)
  coords <- handler |> coordinates()

  expect_s3_class(coords, "tbl_df")
  expect_setequal(
    colnames(coords),
    c("CN", "STATECD", "COUNTYCD", "INVYR", "PLOT", "LON", "LAT")
  )
  expect_equal(nrow(coords), 8)
  expect_true(is.numeric(coords$LON))
  expect_true(is.numeric(coords$LAT))
})

test_that("coordinates() errors when LON/LAT are missing", {
  con <- setup_status_test_db()
  handler <- eval_handler(con, evalid = 1001)

  expect_error(
    handler |> coordinates(),
    "Coordinate column\\(s\\) `LON` and `LAT` not found"
  )
})

test_that("coordinates() accepts custom column names", {
  con <- setup_status_test_db()

  add_coord_cols(con, lon = "LONGITUDE", lat = "LATITUDE")
  DBI::dbExecute(con, "UPDATE PLOT SET LONGITUDE = -72.5, LATITUDE = 43.0")

  handler <- eval_handler(con, evalid = 1001)
  coords <- handler |> coordinates(lon = "LONGITUDE", lat = "LATITUDE")

  expect_setequal(
    colnames(coords),
    c("CN", "STATECD", "COUNTYCD", "INVYR", "PLOT", "LONGITUDE", "LATITUDE")
  )
})

test_that("coordinates() retrieves custom coordinates supplied via augment()", {
  con <- setup_status_test_db()
  handler <- eval_handler(con, evalid = 1001)

  custom <- data.frame(
    CN = c(101, 102, 103, 104, 201, 202, 203, 204),
    LON = seq(-72.5, -72.1, length.out = 8),
    LAT = seq(43.0, 43.4, length.out = 8)
  )

  coords <- suppressWarnings(
    handler |>
      augment(plot(custom, by = "CN")) |>
      coordinates()
  )

  expect_setequal(
    colnames(coords),
    c("CN", "STATECD", "COUNTYCD", "INVYR", "PLOT", "LON", "LAT")
  )
  expect_equal(coords$LON, custom$LON)
  expect_equal(coords$LAT, custom$LAT)
})

test_that("coordinates() respects pending plot-level subsets", {
  con <- setup_status_test_db()

  add_coord_cols(con)
  DBI::dbExecute(con, "UPDATE PLOT SET LON = -72.5, LAT = 43.0")

  handler <- eval_handler(con, evalid = 1001) |>
    subset(plot(CN >= 200))

  coords <- handler |> coordinates()

  expect_equal(nrow(coords), 4)
  expect_true(all(coords$CN >= 200))
})

test_that("coordinates() returns an sf object when as_sf = TRUE", {
  skip_if_not_installed("sf")
  con <- setup_status_test_db()

  add_coord_cols(con)
  DBI::dbExecute(con, "UPDATE PLOT SET LON = -72.5, LAT = 43.0")

  handler <- eval_handler(con, evalid = 1001)
  coords <- handler |> coordinates(as_sf = TRUE)

  expect_s3_class(coords, "sf")
  expect_equal(sf::st_crs(coords)$epsg, 4269)
  expect_true(sf::st_is_longlat(coords))
})

test_that("coordinates() validates the as_sf argument", {
  con <- setup_status_test_db()

  add_coord_cols(con)
  DBI::dbExecute(con, "UPDATE PLOT SET LON = -72.5, LAT = 43.0")

  handler <- eval_handler(con, evalid = 1001)

  expect_error(handler |> coordinates(as_sf = "yes"), "`as_sf` must be `TRUE` or `FALSE`")
  expect_error(handler |> coordinates(lon = ""), "`lon` must be a single non-empty column name")
})
