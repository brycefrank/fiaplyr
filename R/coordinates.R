#' Retrieve Plot Coordinates
#'
#' Returns plot-level coordinates for an [EvalHandler][EvalHandler-class]. By
#' default, the public fuzzed coordinates `LON` and `LAT` from the `PLOT` table
#' are returned. Databases that store coordinates under different column names
#' can pass alternate names via `lon` and `lat`. Alternatively, custom
#' coordinate columns can be attached to the plot table with
#' [augment()][augment] and then retrieved with `coordinates()`:
#'
#' ```
#' handler |>
#'   augment(plot(coords, by = "CN")) |>
#'   coordinates()
#' ```
#'
#' Any pending plot-level [subset()][subset] filters, [transform()][transform]
#' mutations, and [augment()][augment] joins are applied before coordinates are
#' retrieved, so the result reflects the current state of the handler.
#'
#' @param handler A handler object ([EvalHandler][EvalHandler-class] or
#'   [WindowHandler][WindowHandler-class]).
#' @param lon Column name containing longitude coordinates. Defaults to
#'   `"LON"`.
#' @param lat Column name containing latitude coordinates. Defaults to
#'   `"LAT"`.
#' @param as_sf Logical. If `TRUE`, returns an `sf` object with point geometry
#'   built from the coordinate columns. Defaults to `FALSE`.
#' @param crs The EPSG code of the coordinate reference system, used when
#'   `as_sf = TRUE`. Defaults to `4269` (NAD 83), the datum of the public fuzzed
#'   FIA coordinates.
#' @param ... Additional arguments passed to methods.
#'
#' @return A [tibble][tibble::tibble] with the plot identifiers `CN`,
#'   `STATECD`, `COUNTYCD`, `INVYR`, and `PLOT` alongside the coordinate
#'   columns. When `as_sf = TRUE`, an `sf` object with point geometry.
#'
#' @export
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(duckdb::duckdb(), fiadb_vt_mini_path())
#' handler <- eval_handler(con, evalid = 500601)
#'
#' # Default public fuzzed coordinates
#' handler |> coordinates()
#'
#' # As an sf object
#' handler |> coordinates(as_sf = TRUE)
#'
#' # Custom coordinate columns via augment()
#' coords <- data.frame(
#'   CN = c(1, 2, 3),
#'   LON = c(-72.5, -72.6, -72.7),
#'   LAT = c(43.1, 43.0, 42.9)
#' )
#' handler |>
#'   augment(plot(coords, by = "CN")) |>
#'   coordinates()
#' }
setGeneric(
  "coordinates",
  function(handler, lon = "LON", lat = "LAT", as_sf = FALSE, crs = 4269, ...) {
    standardGeneric("coordinates")
  }
)

#' Validate and collect coordinate columns from a plot query
#'
#' Checks that the requested longitude and latitude columns exist in a plot
#' query and are numeric, then collects the plot identifiers alongside them.
#'
#' @param plot_qry A lazy plot query (or data frame) containing plot
#'   identifiers and coordinate columns.
#' @param lon The longitude column name.
#' @param lat The latitude column name.
#' @return A data frame with plot identifiers, `lon`, and `lat`.
#' @keywords internal
.coordinates_from_query <- function(plot_qry, lon, lat) {
  available <- colnames(plot_qry)
  missing_cols <- setdiff(c(lon, lat), available)
  if (length(missing_cols) > 0) {
    rlang::abort(
      sprintf(
        paste0(
          "Coordinate column(s) %s not found in the plot table. ",
          "Use `augment(plot(...))` to attach custom coordinates, or pass ",
          "alternate column names via `lon`/`lat`."
        ),
        paste0("`", missing_cols, "`", collapse = " and ")
      )
    )
  }

  plot_keys <- intersect(c("CN", "STATECD", "COUNTYCD", "INVYR", "PLOT"), available)

  out <- plot_qry %>%
    dplyr::select(dplyr::all_of(c(plot_keys, lon, lat))) %>%
    dplyr::collect()

  if (!is.numeric(out[[lon]])) {
    rlang::abort(sprintf("Coordinate column `%s` is not numeric.", lon))
  }
  if (!is.numeric(out[[lat]])) {
    rlang::abort(sprintf("Coordinate column `%s` is not numeric.", lat))
  }

  out
}

#' Convert a coordinate data frame to an sf object
#'
#' @param out A data frame with `lon` and `lat` columns.
#' @param lon The longitude column name.
#' @param lat The latitude column name.
#' @param crs The EPSG code to assign.
#' @return An sf object with point geometry.
#' @keywords internal
.as_sf_coordinates <- function(out, lon, lat, crs) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(
      "`as_sf = TRUE` requires the 'sf' package. Install it with install.packages('sf').",
      call. = FALSE
    )
  }
  sf::st_as_sf(out, coords = c(lon, lat), crs = crs)
}

.coordinates_check_args <- function(lon, lat, as_sf) {
  if (!is.character(lon) || length(lon) != 1 || is.na(lon) || !nzchar(lon)) {
    rlang::abort("`lon` must be a single non-empty column name.")
  }
  if (!is.character(lat) || length(lat) != 1 || is.na(lat) || !nzchar(lat)) {
    rlang::abort("`lat` must be a single non-empty column name.")
  }
  if (!is.logical(as_sf) || length(as_sf) != 1 || is.na(as_sf)) {
    rlang::abort("`as_sf` must be `TRUE` or `FALSE`.")
  }
}

#' @describeIn coordinates Retrieve plot coordinates for an EvalHandler
#' @export
setMethod(
  "coordinates",
  "EvalHandler",
  function(handler, lon = "LON", lat = "LAT", as_sf = FALSE, crs = 4269, ...) {
    .coordinates_check_args(lon, lat, as_sf)

    out <- .coordinates_from_query(.build_plot_data(handler), lon, lat)

    if (as_sf) {
      out <- .as_sf_coordinates(out, lon, lat, crs)
    }

    out
  }
)

#' @describeIn coordinates Retrieve plot coordinates for a WindowHandler
#' @export
setMethod(
  "coordinates",
  "WindowHandler",
  function(handler, lon = "LON", lat = "LAT", as_sf = FALSE, crs = 4269, ...) {
    .coordinates_check_args(lon, lat, as_sf)

    out <- .coordinates_from_query(.build_plot_data(handler), lon, lat)

    if (as_sf) {
      out <- .as_sf_coordinates(out, lon, lat, crs)
    }

    out
  }
)
