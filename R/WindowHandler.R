#' Class for Spatial Window
#'
#' A handler that connects to a database and subsets the `PLOT` table to a
#' spatial and temporal window. Unlike an [EvalHandler][EvalHandler-class], a
#' `WindowHandler` is not tied to an evaluation: it queries all plots within a
#' window geometry (or bounding box), optionally restricted by state/county and
#' inventory year.
#'
#' @slot tables A list of lazy queries for the tables.
#' @slot pipeline Pending operations grouped by target table and operation.
#' @slot window A list describing the spatial/temporal window that was applied.
#' @slot spec The AnalysisSpec used to build the analysis tables.
#' @export
setClass(
  "WindowHandler",
  contains = "BaseHandler",
  slots = list(
    tables = "list",
    pipeline = "list",
    window = "list",
    spec = "AnalysisSpec"
  )
)

#' Connect to a Spatial Window
#'
#' Subsets the `PLOT` table to a spatial and temporal window. At least one of
#' `geometry`, `bbox`, `statecd`, `county`, or `countycd` should be supplied to
#' meaningfully restrict the plots; `invyrs` further restricts by inventory
#' year. The resulting [WindowHandler][WindowHandler-class] carries a lazy plot
#' query that can be retrieved with [coordinates()][coordinates] and refined
#' with the standard handler verbs.
#'
#' @param db A DBIConnection object.
#' @param geometry An `sf` or `sfc` geometry (point, line, or polygon) defining
#'   the window. Plots are selected via an R-side intersection of their
#'   coordinates against this geometry. Requires the `sf` package.
#' @param crs The EPSG code of `geometry` (or `bbox`). If `NULL` and
#'   `geometry` carries a coordinate reference system, it is used.
#' @param bbox A numeric vector of length 4 giving `c(xmin, ymin, xmax, ymax)`
#'   in the coordinate system of the plot coordinates (longitude/latitude for
#'   the default public fuzzed coordinates).
#' @param invyrs A numeric vector of inventory years to retain. Defaults to all
#'   years.
#' @param statecd A numeric vector of state codes. When used alone, retains all
#'   plots in those states.
#' @param countycd A numeric vector of county codes. Only valid in combination
#'   with a single-valued `statecd` (county codes are ambiguous across states).
#' @param county A data frame with `STATECD` and `COUNTYCD` columns, one row per
#'   county. Use this when selecting counties across multiple states.
#' @param spec An [AnalysisSpec][AnalysisSpec-class] object controlling how the
#'   selected plots are aggregated. Defaults to [status_analysis()][status_analysis].
#' @param backend An optional [database_mapping()][database_mapping] for custom
#'   schema/table names.
#'
#' @return A [WindowHandler][WindowHandler-class] connected to the database with
#'   the plot query restricted to the requested window.
#' @export
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(duckdb::duckdb(), fiadb_vt_mini_path())
#'
#' # Whole states
#' window_handler(con, statecd = 50)
#'
#' # One state, several counties
#' window_handler(con, statecd = 50, countycd = c(5, 1, 3))
#'
#' # Counties across states
#' counties <- data.frame(STATECD = c(50, 50, 9), COUNTYCD = c(5, 1, 3))
#' window_handler(con, county = counties)
#'
#' # Spatial window
#' win <- sf::st_read("windham.shp") # in a projected CRS
#' window_handler(con, geometry = win, crs = 26918)
#'
#' # With a temporal range
#' window_handler(con, statecd = 50, invyrs = 2008:2012)
#'
#' # Composed with a GRM analysis spec
#' window_handler(con, statecd = 50, spec = grm_analysis()) |>
#'   aggregate(tree_history(grm_mortality()))
#' }
window_handler <- function(
  db,
  geometry = NULL,
  crs = NULL,
  bbox = NULL,
  invyrs = NULL,
  statecd = NULL,
  countycd = NULL,
  county = NULL,
  spec = status_analysis(),
  backend = NULL
) {
  if (is.null(backend)) {
    backend <- database_mapping()
  }

  tbl_ref <- function(name) get_table_ref(backend, name)
  plot_qry <- dplyr::tbl(db, tbl_ref("PLOT"))

  window_spec <- list(
    geometry = NULL,
    crs = NULL,
    bbox = NULL,
    invyrs = NULL,
    statecd = NULL,
    countycd = NULL,
    county = NULL
  )

  if (!is.null(geometry) && !is.null(bbox)) {
    rlang::abort("Provide either `geometry` or `bbox`, not both.")
  }

  if (!is.null(county) && !is.null(countycd)) {
    rlang::abort("Provide either `county` or `countycd`, not both.")
  }

  if (!is.null(countycd) && is.null(statecd)) {
    rlang::abort("`countycd` is ambiguous without `statecd`.")
  }

  if (!is.null(countycd) && length(statecd) != 1) {
    rlang::abort(
      paste0(
        "`countycd` is only valid with a single-valued `statecd`. ",
        "Use the `county` table when selecting counties across multiple states."
      )
    )
  }

  if (!is.null(bbox) && (length(bbox) != 4 || !is.numeric(bbox))) {
    rlang::abort("`bbox` must be a numeric vector of length 4: c(xmin, ymin, xmax, ymax).")
  }

  if (!is.null(county)) {
    if (!all(c("STATECD", "COUNTYCD") %in% names(county))) {
      rlang::abort("`county` must have `STATECD` and `COUNTYCD` columns.")
    }
  }

  # State/county filters (database-side)
  if (!is.null(statecd)) {
    plot_qry <- plot_qry %>%
      dplyr::filter(STATECD %in% !!statecd)
    window_spec$statecd <- statecd
  }

  if (!is.null(countycd)) {
    plot_qry <- plot_qry %>%
      dplyr::filter(COUNTYCD %in% !!countycd)
    window_spec$countycd <- countycd
  }

  if (!is.null(county)) {
    plot_qry <- plot_qry %>%
      dplyr::semi_join(county, by = c("STATECD", "COUNTYCD"), copy = TRUE)
    window_spec$county <- county
  }

  # Temporal filter (database-side)
  if (!is.null(invyrs)) {
    plot_qry <- plot_qry %>%
      dplyr::filter(INVYR %in% !!invyrs)
    window_spec$invyrs <- invyrs
  }

  # Bounding-box filter (database-side)
  if (!is.null(bbox)) {
    plot_qry <- plot_qry %>%
      dplyr::filter(
        LON >= !!bbox[[1]],
        LON <= !!bbox[[3]],
        LAT >= !!bbox[[2]],
        LAT <= !!bbox[[4]]
      )
    window_spec$bbox <- bbox
  }

  # Geometry intersection (R-side)
  if (!is.null(geometry)) {
    geom <- .as_window_geometry(geometry)
    resolved_crs <- .resolve_window_crs(geom, crs)
    window_spec$geometry <- geometry
    window_spec$crs <- resolved_crs

    coords <- .coordinates_from_query(plot_qry, "LON", "LAT")

    pts <- sf::st_as_sf(coords, coords = c("LON", "LAT"), crs = 4269)
    pts <- sf::st_transform(pts, crs = resolved_crs)

    keep <- lengths(sf::st_intersects(pts, geom, sparse = TRUE)) > 0
    matched_cn <- coords$CN[keep]

    plot_qry <- plot_qry %>%
      dplyr::filter(CN %in% !!matched_cn)
  }

  spec_tables <- build_tables(spec, plot_qry, db, backend = backend, evalid = NULL)

  new(
    "WindowHandler",
    db = db,
    tables = spec_tables,
    pipeline = .new_pipeline(),
    window = window_spec,
    spec = spec
  )
}

#' Extract an sfc geometry from an sf object
#'
#' @param geometry An `sf` data frame or `sfc` geometry.
#' @return An `sfc` geometry.
#' @keywords internal
.as_window_geometry <- function(geometry) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(
      "`geometry` requires the 'sf' package. Install it with install.packages('sf').",
      call. = FALSE
    )
  }
  if (inherits(geometry, "sf")) {
    return(sf::st_geometry(geometry))
  }
  if (inherits(geometry, "sfc")) {
    return(geometry)
  }
  rlang::abort("`geometry` must be an `sf` or `sfc` object.")
}

#' Resolve the CRS for a window geometry
#'
#' @param geom An `sfc` geometry.
#' @param crs An optional EPSG code.
#' @return An EPSG code.
#' @keywords internal
.resolve_window_crs <- function(geom, crs) {
  if (!is.null(crs)) {
    if (!is.numeric(crs) || length(crs) != 1 || is.na(crs)) {
      rlang::abort("`crs` must be a single EPSG code.")
    }
    return(crs)
  }

  geom_crs <- sf::st_crs(geom)
  epsg <- if (is.na(geom_crs)) NA_integer_ else geom_crs$epsg

  if (is.na(epsg)) {
    rlang::abort(
      paste0(
        "Could not determine the CRS of `geometry`. ",
        "Supply the EPSG code via `crs`."
      )
    )
  }

  epsg
}

#' Disable estimation for WindowHandler
#'
#' `estimate()` is intentionally not supported for `WindowHandler` yet; it will
#' be added in a later feature release. These methods exist so that users
#' receive a clear error instead of an S4 dispatch failure.
#' @noRd
.stop_estimate_unsupported <- function() {
  rlang::abort(
    "estimation is not yet supported for `WindowHandler`.",
    class = "fiaplyr_estimate_unsupported"
  )
}

#' @noRd
setMethod(
  "estimate",
  signature(object = "WindowHandler", estimator = "missing"),
  function(object, ..., output = "mean", margins = FALSE) {
    .stop_estimate_unsupported()
  }
)

#' @noRd
setMethod(
  "estimate",
  signature(object = "WindowHandler", estimator = "ANY"),
  function(
    object,
    ...,
    output = "mean",
    margins = FALSE,
    estimator = "auto",
    var_est = "auto"
  ) {
    .stop_estimate_unsupported()
  }
)

#' Show Method for WindowHandler
#'
#' @param object A WindowHandler object.
#' @export
setMethod("show", "WindowHandler", function(object) {
  cat("WindowHandler\n")
  cat("-------------\n")

  n_plots <- object@tables$plot %>%
    dplyr::tally() %>%
    dplyr::collect() %>%
    dplyr::pull(n)

  invyr_range <- object@tables$plot %>%
    dplyr::summarise(
      min_invyr = min(INVYR, na.rm = TRUE),
      max_invyr = max(INVYR, na.rm = TRUE)
    ) %>%
    dplyr::collect()

  cat("Plots:          ", n_plots, "\n")
  if (!is.na(invyr_range$min_invyr)) {
    cat("Inventory Years:", invyr_range$min_invyr, "-", invyr_range$max_invyr, "\n")
  }

  spec <- object@window

  if (!is.null(spec$statecd)) {
    cat("States:         ", paste(spec$statecd, collapse = ", "), "\n")
  }
  if (!is.null(spec$countycd)) {
    cat("Counties:       ", paste(spec$countycd, collapse = ", "), "\n")
  }
  if (!is.null(spec$county)) {
    n_counties <- nrow(spec$county)
    cat("Counties:       ", n_counties, "counties (from `county` table)\n")
  }
  if (!is.null(spec$bbox)) {
    cat("Bounding box:   ", paste(round(spec$bbox, 4), collapse = ", "), "\n")
  }
  if (!is.null(spec$geometry)) {
    cat("Geometry:       ", "sf geometry in EPSG:", spec$crs, "\n")
  }
})
