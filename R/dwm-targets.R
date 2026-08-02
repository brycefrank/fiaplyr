.dwm_component_helpers <- c(
  "dwm_cwd", "dwm_fwd", "dwm_pile", "dwm_fuel", "dwm_duff", "dwm_litter"
)

.dwm_support <- list(
  CWD = list(
    VOLCF = list(base = "CWD_VOLCF", units = "cubic feet per acre", scale = 1),
    DRYBIO = list(base = "CWD_DRYBIO", units = "dry short tons per acre", scale = 1 / 2000),
    CARBON = list(base = "CWD_CARBON", units = "short tons per acre", scale = 1 / 2000),
    LPA = list(base = "CWD_LPA", units = "pieces per acre", scale = 1)
  ),
  FWD = list(
    VOLCF = list(base = "FWD_{size}_VOLCF", units = "cubic feet per acre", scale = 1),
    DRYBIO = list(base = "FWD_{size}_DRYBIO", units = "dry short tons per acre", scale = 1 / 2000),
    CARBON = list(base = "FWD_{size}_CARBON", units = "short tons per acre", scale = 1 / 2000)
  ),
  PILE = list(
    VOLCF = list(base = "PILE_VOLCF", units = "cubic feet per acre", scale = 1),
    DRYBIO = list(base = "PILE_DRYBIO", units = "dry short tons per acre", scale = 1 / 2000),
    CARBON = list(base = "PILE_CARBON", units = "short tons per acre", scale = 1 / 2000)
  ),
  FUEL = list(
    DRYBIO = list(column = "FUEL_BIOMASS", units = "dry short tons per acre", scale = 1 / 2000),
    CARBON = list(column = "FUEL_CARBON", units = "short tons per acre", scale = 1 / 2000)
  ),
  DUFF = list(
    DRYBIO = list(column = "DUFF_BIOMASS", units = "dry short tons per acre", scale = 1 / 2000),
    CARBON = list(column = "DUFF_CARBON", units = "short tons per acre", scale = 1 / 2000)
  ),
  LITTER = list(
    DRYBIO = list(column = "LITTER_BIOMASS", units = "dry short tons per acre", scale = 1 / 2000),
    CARBON = list(column = "LITTER_CARBON", units = "short tons per acre", scale = 1 / 2000)
  )
)

.dwm_target <- function(component, ..., size = NULL) {
  component <- toupper(component)
  attributes <- rlang::enquos(...)

  if (length(attributes) != 1) {
    stop(
      sprintf("`dwm_%s()` requires exactly one attribute.", tolower(component)),
      call. = FALSE
    )
  }

  attribute_expr <- rlang::quo_get_expr(attributes[[1]])
  if (!rlang::is_symbol(attribute_expr)) {
    stop("DWM attributes must be supplied as bare names.", call. = FALSE)
  }
  attribute <- toupper(rlang::as_string(attribute_expr))

  valid_attributes <- names(.dwm_support[[component]])
  if (!attribute %in% valid_attributes) {
    stop(
      sprintf(
        "Invalid attribute `%s` for %s. Valid choices are: %s.",
        attribute,
        component,
        paste(valid_attributes, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (identical(component, "FWD")) {
    if (is.null(size) || length(size) != 1 || is.na(size)) {
      stop("`dwm_fwd()` requires `size` to be one of: SM, MD, LG, ALL.", call. = FALSE)
    }
    size <- toupper(as.character(size))
    if (!size %in% c("SM", "MD", "LG", "ALL")) {
      stop("Invalid FWD size. Valid choices are: SM, MD, LG, ALL.", call. = FALSE)
    }
  } else if (!is.null(size)) {
    stop("`size` is only supported by `dwm_fwd()`.", call. = FALSE)
  }

  supplied_name <- names(attributes)
  supplied_name <- if (
    !is.null(supplied_name) &&
      length(supplied_name) == 1 &&
      nzchar(supplied_name)
  ) {
    supplied_name
  } else {
    ""
  }

  default_name <- paste0("dwm_", tolower(component))
  if (identical(component, "FWD")) {
    default_name <- paste0(default_name, "_", tolower(size))
  }
  default_name <- paste0(default_name, "_", attribute)

  target <- structure(
    list(
      component = component,
      attribute = attribute,
      size = size,
      name = if (nzchar(supplied_name)) supplied_name else default_name
    ),
    class = "fiaplyr_dwm_target"
  )

  structure(
    list(target),
    target_table = "dwm",
    class = c("fiaplyr_dwm_helper", "list")
  )
}

.resolve_dwm_columns <- function(target, adjusted) {
  if (!inherits(target, "fiaplyr_dwm_target")) {
    stop("Invalid DWM aggregation target.", call. = FALSE)
  }

  entry <- .dwm_support[[target$component]][[target$attribute]]
  if (!is.null(entry$column)) {
    return(entry$column)
  }

  bases <- if (identical(target$component, "FWD")) {
    sizes <- if (identical(target$size, "ALL")) c("SM", "MD", "LG") else target$size
    vapply(
      sizes,
      function(size) sub("\\{size\\}", size, entry$base),
      character(1)
    )
  } else {
    entry$base
  }

  paste0(bases, if (adjusted) "_ADJ" else "_UNADJ")
}

.dwm_target_scale <- function(target) {
  .dwm_support[[target$component]][[target$attribute]]$scale
}

#' Select Coarse Woody Debris
#'
#' Select one coarse woody debris attribute from `COND_DWM_CALC`. Supported
#' attributes are:
#'
#' - `VOLCF`: cubic feet per acre
#' - `DRYBIO`: dry short tons per acre
#' - `CARBON`: short tons per acre
#' - `LPA`: pieces per acre
#'
#' @param ... Exactly one bare attribute, optionally named to control the output
#'   column.
#' @return A structured DWM target for [aggregate()] or [estimate()].
#' @export
dwm_cwd <- function(...) {
  .dwm_target("CWD", ...)
}

#' Select Fine Woody Debris
#'
#' Select one fine woody debris attribute. Supported attributes are:
#'
#' - `VOLCF`: cubic feet per acre
#' - `DRYBIO`: dry short tons per acre
#' - `CARBON`: short tons per acre
#'
#' @param ... Exactly one bare attribute, optionally named to control the output
#'   column.
#' @param size Fine woody debris size class: `"SM"`, `"MD"`, `"LG"`, or
#'   `"ALL"` to sum all three classes.
#' @return A structured DWM target for [aggregate()] or [estimate()].
#' @export
dwm_fwd <- function(..., size = NULL) {
  .dwm_target("FWD", ..., size = size)
}

#' Select Residual Piles
#'
#' Select one pile attribute. Supported attributes are:
#'
#' - `VOLCF`: cubic feet per acre
#' - `DRYBIO`: dry short tons per acre
#' - `CARBON`: short tons per acre
#'
#' @inheritParams dwm_cwd
#' @return A structured DWM target for [aggregate()] or [estimate()].
#' @export
dwm_pile <- function(...) {
  .dwm_target("PILE", ...)
}

#' Select Fuel Loading
#'
#' Select one fuel loading attribute. Supported attributes are:
#'
#' - `DRYBIO`: dry short tons per acre
#' - `CARBON`: short tons per acre
#'
#' FIADB stores the biomass field as `FUEL_BIOMASS`.
#'
#' @inheritParams dwm_cwd
#' @return A structured DWM target for [aggregate()] or [estimate()].
#' @export
dwm_fuel <- function(...) {
  .dwm_target("FUEL", ...)
}

#' Select Duff Loading
#'
#' Select one duff loading attribute. Supported attributes are:
#'
#' - `DRYBIO`: dry short tons per acre
#' - `CARBON`: short tons per acre
#'
#' FIADB stores the biomass field as `DUFF_BIOMASS`.
#'
#' @inheritParams dwm_cwd
#' @return A structured DWM target for [aggregate()] or [estimate()].
#' @export
dwm_duff <- function(...) {
  .dwm_target("DUFF", ...)
}

#' Select Litter Loading
#'
#' Select one litter loading attribute. Supported attributes are:
#'
#' - `DRYBIO`: dry short tons per acre
#' - `CARBON`: short tons per acre
#'
#' FIADB stores the biomass field as `LITTER_BIOMASS`.
#'
#' @inheritParams dwm_cwd
#' @return A structured DWM target for [aggregate()] or [estimate()].
#' @export
dwm_litter <- function(...) {
  .dwm_target("LITTER", ...)
}
