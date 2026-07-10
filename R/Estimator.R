#' Base Estimator Class
#'
#' @slot handler A EvalHandler object.
#' @export
setClass("Estimator",
  slots = list(
    handler = "ANY"
  )
)

#' Base Variance Estimator Class
#'
#' @export
setClass("VarianceEstimator", contains = "VIRTUAL")

#' Taylor Variance Estimator
#'
#' A variance-estimator specification for Taylor linearization.
#'
#' @export
setClass("TaylorVarianceEstimator", contains = "VarianceEstimator")

#' Configure Taylor Variance Estimation
#'
#' @return A `TaylorVarianceEstimator` object.
#' @export
ve_taylor <- function() {
  new("TaylorVarianceEstimator")
}

setGeneric(
  ".estimate_composed",
  function(
    point_estimator,
    variance_estimator,
    handler,
    target,
    ...,
    output = "mean",
    margins = FALSE
  ) {
    standardGeneric(".estimate_composed")
  },
  signature = c("point_estimator", "variance_estimator")
)

#' Get Subplot Type Adjustment Factors
#'
#' @param object A EvalHandler object.
#' @param subptypes A character vector of subplot types
#' @keywords internal
.get_subptype_adjustment_factors <- function(object, subptypes = c("MACR", "SUBP", "MICR")) {
  subptype_vars <- paste0("ADJ_FACTOR_", subptypes)

  object@tables$pop_stratum %>%
    dplyr::select(
      CN,
      dplyr::all_of(subptype_vars)
    ) |>
    dplyr::rename(
      STRATUM_CN = CN
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(subptype_vars),
      names_to = "SUBPTYPE",
      names_prefix = "ADJ_FACTOR_",
      values_to = "ADJ_FACTOR"
    )
}

#' Get Strata Summary
#'
#' Calculates the strata summary from the population tables.
#'
#' @param handler A EvalHandler object.
#' @return A dataframe with strata summary statistics.
#' @keywords internal
.get_strata_summary <- function(handler) {
  handler@tables$pop_stratum %>%
    dplyr::inner_join(
      handler@tables$pop_estn_unit,
      by = c("ESTN_UNIT_CN" = "CN"),
      suffix = c("", ".eu")
    ) %>%
    dplyr::group_by(
      ESTN_UNIT_CN
    ) %>%
    dplyr::mutate(
      n = sum(P2POINTCNT, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      w_h = as.numeric(P1POINTCNT) / P1PNTCNT_EU, n_h = P2POINTCNT
    ) %>%
    dplyr::select(
      STRATUM_CN = CN, ESTN_UNIT_CN, w_h, n_h, n
    )
}
