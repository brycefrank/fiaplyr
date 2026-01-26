#' PostStratifiedRatioEstimator Class
#'
#' @slot numerator A EvalHandler object for the numerator.
#' @slot denominator A EvalHandler object for the denominator.
#' @slot strata_weights A dataframe containing strata weights.
#' @export
setClass("PostStratifiedRatioEstimator",
  contains = "Estimator",
  slots = list(
    numerator = "EvalHandler",
    denominator = "EvalHandler",
    strata_weights = "ANY"
  )
)

#' Constructor for PostStratifiedRatioEstimator
#'
#' @param numerator A EvalHandler object for the numerator.
#' @param denominator A EvalHandler object for the denominator. Defaults to numerator.
#' @export
PostStratifiedRatioEstimator <- function(numerator, denominator = numerator) {
  # Check if EVALIDs match
  if (evalid(numerator) != evalid(denominator)) {
    stop("Numerator and denominator must have the same EVALID.")
  }

  # Calculate strata weights (using numerator, as they share EVALID)
  strata_weights <- numerator@pop_stratum %>%
    dplyr::inner_join(
      numerator@pop_estn_unit,
      by = c("ESTN_UNIT_CN" = "CN"),
      suffix = c("", ".eu")
    ) %>%
    dplyr::mutate(
      w_h = as.numeric(P1POINTCNT) / P1PNTCNT_EU
    ) %>%
    dplyr::select(
      STRATUM_CN = CN, ESTN_UNIT_CN, w_h, P2POINTCNT, AREA_USED
    )

  new("PostStratifiedRatioEstimator",
    numerator = numerator,
    denominator = denominator,
    strata_weights = strata_weights
  )
}

#' Estimate Ratio
#'
#' @param object A PostStratifiedRatioEstimator object.
#' @param ... Ratio formulas.
#' @export
setGeneric("estimate_ratio", function(object, ...) standardGeneric("estimate_ratio"))

#' @describeIn estimate_ratio Estimate ratio for PostStratifiedRatioEstimator
setMethod("estimate_ratio", "PostStratifiedRatioEstimator", function(object, ...) {
  # Boilerplate for now
  stop("Not implemented yet")
})
