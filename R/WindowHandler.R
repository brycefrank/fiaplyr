#' Define a Spatial Window
#'
#' @param states State abbreviations or numeric FIA state codes.
#' @param counties Optional numeric FIA county codes.
#' @param polygon Optional `sf` or `sfc` polygon used to select plots.
#' @return A spatial window specification.
#' @export
spatial_window <- function(states = NULL, counties = NULL, polygon = NULL) {
  if (inherits(states, "sf") || inherits(states, "sfc")) {
    if (!is.null(polygon)) {
      stop("Supply a polygon either as `states` or `polygon`, not both.", call. = FALSE)
    }
    polygon <- states
    states <- NULL
  }

  if (!is.null(polygon) &&
    !(inherits(polygon, "sf") || inherits(polygon, "sfc"))) {
    stop("`polygon` must be an sf or sfc object.", call. = FALSE)
  }

  structure(
    list(states = states, counties = counties, polygon = polygon, years = NULL, type = NULL),
    class = "fiaplyr_window"
  )
}

#' Define a Temporal Window
#'
#' @param years Inventory or measurement years to include.
#' @param type Whether `years` applies to inventory or measurement years.
#' @return A temporal window specification.
#' @export
temporal_window <- function(years, type = c("measurement", "inventory")) {
  type <- match.arg(type)
  if (!is.numeric(years) || length(years) == 0 || anyNA(years)) {
    stop("`years` must be a non-empty numeric vector.", call. = FALSE)
  }

  structure(
    list(states = NULL, counties = NULL, polygon = NULL, years = years, type = type),
    class = "fiaplyr_window"
  )
}

#' @export
"&.fiaplyr_window" <- function(e1, e2) {
  if (!inherits(e1, "fiaplyr_window") || !inherits(e2, "fiaplyr_window")) {
    stop("Both operands must be fiaplyr window specifications.", call. = FALSE)
  }

  merge_component <- function(name) {
    left <- e1[[name]]
    right <- e2[[name]]
    if (!is.null(left) && !is.null(right)) {
      stop("A window may define `", name, "` only once.", call. = FALSE)
    }
    if (is.null(left)) right else left
  }

  structure(
    list(
      states = merge_component("states"),
      counties = merge_component("counties"),
      polygon = merge_component("polygon"),
      years = merge_component("years"),
      type = merge_component("type")
    ),
    class = "fiaplyr_window"
  )
}

.window_state_codes <- function(states) {
  if (is.null(states) || is.numeric(states)) {
    return(states)
  }

  states <- toupper(states)
  state_codes <- c(
    AL = 1, AK = 2, AZ = 4, AR = 5, CA = 6, CO = 8, CT = 9, DE = 10,
    FL = 12, GA = 13, HI = 15, ID = 16, IL = 17, IN = 18, IA = 19,
    KS = 20, KY = 21, LA = 22, ME = 23, MD = 24, MA = 25, MI = 26,
    MN = 27, MS = 28, MO = 29, MT = 30, NE = 31, NV = 32, NH = 33,
    NJ = 34, NM = 35, NY = 36, NC = 37, ND = 38, OH = 39, OK = 40,
    OR = 41, PA = 42, RI = 44, SC = 45, SD = 46, TN = 47, TX = 48,
    UT = 49, VT = 50, VA = 51, WA = 53, WV = 54, WI = 55, WY = 56
  )
  codes <- unname(state_codes[states])
  if (anyNA(codes)) {
    stop("`states` must contain valid state abbreviations or FIA state codes.", call. = FALSE)
  }
  codes
}

.window_plot_selection <- function(db, window, backend) {
  if (!inherits(window, "fiaplyr_window")) {
    stop("`window` must be created with `spatial_window()` and/or `temporal_window()`.", call. = FALSE)
  }

  plot_qry <- dplyr::tbl(db, get_table_ref(backend, "PLOT"))
  columns <- colnames(plot_qry)
  if (!is.null(window$states)) {
    if (!"STATECD" %in% columns) {
      stop("PLOT is missing STATECD required by `spatial_window(states = ...)`.", call. = FALSE)
    }
    plot_qry <- dplyr::filter(plot_qry, STATECD %in% !!.window_state_codes(window$states))
  }
  if (!is.null(window$counties)) {
    if (!"COUNTYCD" %in% columns) {
      stop("PLOT is missing COUNTYCD required by `spatial_window(counties = ...)`.", call. = FALSE)
    }
    plot_qry <- dplyr::filter(plot_qry, COUNTYCD %in% !!window$counties)
  }
  if (!is.null(window$years)) {
    year_col <- if (identical(window$type, "measurement")) "MEASYEAR" else "INVYR"
    if (!year_col %in% columns) {
      stop("PLOT is missing ", year_col, " required by `temporal_window()`.", call. = FALSE)
    }
    plot_qry <- dplyr::filter(plot_qry, .data[[year_col]] %in% !!window$years)
  }

  if (!is.null(window$polygon)) {
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop("Polygon windows require the sf package.", call. = FALSE)
    }
    coord_cols <- if (all(c("LON", "LAT") %in% columns)) {
      c("LON", "LAT")
    } else if (all(c("LON", "LATITUDE") %in% columns)) {
      c("LON", "LATITUDE")
    } else {
      stop("Polygon windows require PLOT longitude and latitude columns named LON/LAT.", call. = FALSE)
    }
    coords <- plot_qry %>%
      dplyr::select(CN, dplyr::all_of(coord_cols)) %>%
      dplyr::collect()
    points <- sf::st_as_sf(coords, coords = coord_cols, crs = 4326, remove = FALSE)
    polygon <- sf::st_transform(window$polygon, 4326)
    selected <- coords[lengths(sf::st_intersects(points, polygon)) > 0, "CN", drop = FALSE]
    plot_qry <- dplyr::filter(plot_qry, CN %in% !!selected$CN)
  }

  structure(list(plot = plot_qry), class = "fiaplyr_plot_selection")
}

#' Class for Spatiotemporal Window Pipelines
#'
#' @slot window The spatiotemporal window specification.
#' @export
setClass(
  "WindowHandler",
  contains = "EvalHandler",
  slots = list(window = "list")
)

#' Connect to a Spatiotemporal Window
#'
#' Creates a handler over plots selected by a spatial and/or temporal window.
#' Window handlers support the same transformation, subsetting, partitioning,
#' and aggregation workflows as evaluation handlers. Post-stratified estimation
#' is unavailable because the window does not define estimation strata.
#'
#' @param db A DBIConnection object.
#' @param spec An AnalysisSpec object. Defaults to [status_analysis()].
#' @param window A window created by [spatial_window()] and/or [temporal_window()].
#' @param backend Optional DatabaseMapping for custom schema/table names.
#' @return A WindowHandler.
#' @export
window_handler <- function(db, spec = status_analysis(), window, backend = NULL) {
  if (is.null(backend)) {
    backend <- database_mapping()
  }
  selection <- .window_plot_selection(db, window, backend)
  tables <- initialize_tables(spec, db, selection, backend)

  new(
    "WindowHandler",
    db = db,
    evalid = NA_real_,
    window = window,
    tables = tables,
    spec = spec,
    internal_cache = new.env(parent = emptyenv()),
    plot_mutations = list(),
    plot_filters = list(),
    plot_domains = list(),
    tree_mutations = list(),
    cond_mutations = list(),
    tree_history_mutations = list(),
    tree_domains = list(),
    cond_domains = list(),
    tree_history_domains = list(),
    tree_filters = list(),
    cond_filters = list(),
    tree_history_filters = list(),
    plot_augmentations = list(),
    tree_augmentations = list(),
    cond_augmentations = list(),
    tree_history_augmentations = list()
  )
}

#' @describeIn estimate Post-stratified estimation is unavailable for a window.
setMethod("estimate", "WindowHandler", function(object, ..., output = "mean", margins = FALSE) {
  stop(
    "`estimate()` is not available for WindowHandler because a spatiotemporal window has no estimation strata.",
    call. = FALSE
  )
})

#' @describeIn evalid Window handlers do not have an evaluation ID.
setMethod("evalid", "WindowHandler", function(handler) {
  NA_real_
})

#' @describeIn summary Summary for a WindowHandler
setMethod("summary", "WindowHandler", function(object) {
  n_plots <- object@tables$plot %>%
    dplyr::summarise(n_plots = dplyr::n()) %>%
    dplyr::collect() %>%
    dplyr::pull(n_plots)

  c(list(n_plots = n_plots), spec_summary_fields(object@spec, object))
})

#' @describeIn show Show a WindowHandler
setMethod("show", "WindowHandler", function(object) {
  cat("WindowHandler\n")
  cat("-------------\n")
  cat("Plots:          ", summary(object)$n_plots, "\n")
})
