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
  args <- list(...)
  if (length(args) == 0) stop("Must provide a formula.")
  formula <- args[[1]]

  parsed <- parse_ratio_formula(formula)

  if (parsed$slot != "tree") {
     stop("Only 'tree' slot is supported for ratio estimates currently.")
  }

  ratios <- parsed$ratios

  # Identify all unique numerator and denominator variables
  all_nums <- unique(sapply(ratios, function(x) x$numerator))
  all_dens <- unique(sapply(ratios, function(x) x$denominator))

  # Estimate means for all numerators
  num_est <- estimate_tree_means(object@numerator, all_nums)

  # Estimate means for all denominators
  den_est <- estimate_tree_means(object@denominator, all_dens)

  # Identify group vars
  # Get group vars from num_est (excluding ESTN_UNIT_CN and targets)
  num_cols <- colnames(num_est)
  group_vars <- setdiff(num_cols, c("ESTN_UNIT_CN", all_nums))

  # Rename columns to avoid collisions and clarify source
  num_est <- num_est |> dplyr::rename_with(~ paste0(., ".num"), dplyr::all_of(all_nums))
  den_est <- den_est |> dplyr::rename_with(~ paste0(., ".den"), dplyr::all_of(all_dens))

  # Join
  combined <- num_est |>
    dplyr::inner_join(den_est, by = c("ESTN_UNIT_CN", group_vars))

  # Calculate each ratio
  for (r in ratios) {
    n_var <- r$numerator
    d_var <- r$denominator
    # Naming convention: R_NUM_DEN
    ratio_name <- paste0("R_", n_var, "_", d_var)

    combined <- combined |>
       dplyr::mutate(!!ratio_name := .data[[paste0(n_var, ".num")]] / .data[[paste0(d_var, ".den")]])
  }

  # Select only key columns and ratio columns
  # But maybe user wants components too? For now, just return ratios + keys.
  ratio_cols <- sapply(ratios, function(r) paste0("R_", r$numerator, "_", r$denominator))

  combined |>
    dplyr::select(dplyr::all_of(c("ESTN_UNIT_CN", group_vars, ratio_cols)))
})
