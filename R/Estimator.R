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

#' Post-Stratified Variance Estimator
#'
#' A variance-estimator specification for standard non-ratio post-stratified
#' estimation.
#'
#' @export
setClass("PostStratifiedVarianceEstimator", contains = "VarianceEstimator")

#' Post-Stratified Ratio Variance Estimator
#'
#' A variance-estimator specification for standard ratio post-stratified
#' estimation.
#'
#' @export
setClass("PostStratifiedRatioVarianceEstimator", contains = "VarianceEstimator")

#' Configure Post-Stratified Variance Estimation
#'
#' @return A `PostStratifiedVarianceEstimator` object.
#' @export
ve_post_strat <- function() {
  new("PostStratifiedVarianceEstimator")
}

#' Configure Post-Stratified Ratio Variance Estimation
#'
#' @return A `PostStratifiedRatioVarianceEstimator` object.
#' @export
ve_post_strat_ratio <- function() {
  new("PostStratifiedRatioVarianceEstimator")
}

.resolve_post_strat_var_est <- function(var_est = "auto", context = c("non_ratio", "ratio")) {
  context <- match.arg(context)

  if (is.character(var_est) && length(var_est) == 1) {
    if (!identical(var_est, "auto")) {
      stop("`var_est` must be a VarianceEstimator object or the string `\"auto\"`.", call. = FALSE)
    }

    if (identical(context, "non_ratio")) {
      return(ve_post_strat())
    }
    return(ve_post_strat_ratio())
  }

  if (!inherits(var_est, "VarianceEstimator")) {
    stop("`var_est` must be a VarianceEstimator object or the string `\"auto\"`.", call. = FALSE)
  }

  expected_class <- if (identical(context, "non_ratio")) {
    "PostStratifiedVarianceEstimator"
  } else {
    "PostStratifiedRatioVarianceEstimator"
  }

  if (!inherits(var_est, expected_class)) {
    stop(
      "`var_est` must match estimator context. Use `ve_post_strat()` for non-ratio estimates and `ve_post_strat_ratio()` for ratio estimates.",
      call. = FALSE
    )
  }

  var_est
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
