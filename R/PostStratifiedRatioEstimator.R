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

  new("PostStratifiedRatioEstimator",
    numerator = numerator,
    denominator = denominator,
    strata_weights = get_strata_weights(numerator)
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
  if (length(args) != 2) stop("Must provide exactly two formulas (numerator, denominator).")

  f_num <- args[[1]]
  f_den <- args[[2]]

  # 1. Parse formulas and aggregate plot-level data for each side
  parsed_num <- parse_formula(f_num)
  parsed_den <- parse_formula(f_den)

  agg_num <- .psr_aggregate(object@numerator, parsed_num)
  agg_den <- .psr_aggregate(object@denominator, parsed_den)

  # 2. Resolve value column names
  vals_num <- .psr_val_cols(parsed_num)
  vals_den <- .psr_val_cols(parsed_den)

  # 3. Join strata once for each side - reused by both the variance and covariance pipelines
  strata_num <- .ps_join_strata(agg_num, object@numerator)
  strata_den <- .ps_join_strata(agg_den, object@denominator)

  # 4. Stats pipeline for each side, producing [domain_vars, var, estimate, se]
  stats_num <- strata_num %>%
    .ps_strata_stats(vals_num) %>%
    .ps_eu_stats(vals_num) %>%
    .ps_pop_stats(object@numerator, vals_num)

  stats_den <- strata_den %>%
    .ps_strata_stats(vals_den) %>%
    .ps_eu_stats(vals_den) %>%
    .ps_pop_stats(object@denominator, vals_den)

  # 5. Covariance pipeline.
  # Build a lookup table mapping each cov column name to its (var_n, var_d) pair.
  # Row-major order: i (numerator) is the outer loop, j (denominator) is the inner loop.
  cov_pair_df <- data.frame(
    var_n = rep(vals_num, each = length(vals_den)),
    var_d = rep(vals_den, times = length(vals_num)),
    stringsAsFactors = FALSE
  )
  cov_cols <- paste0(".cov_", seq_len(nrow(cov_pair_df)))
  cov_pair_df$cov_col <- cov_cols

  pop_cov <- strata_num %>%
    .ps_strata_cov(strata_den, vals_num, vals_den, cov_cols) %>%
    .ps_eu_cov(cov_cols) %>%
    .ps_pop_cov(object@numerator, cov_cols)

  # Pivot pop_cov to long format using the known lookup (avoids fragile name parsing)
  pop_cov_long <- pop_cov %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(cov_cols),
      names_to = "cov_col",
      values_to = "cov_val"
    ) %>%
    dplyr::left_join(cov_pair_df, by = "cov_col") %>%
    dplyr::select(-cov_col)
  # Columns: [domain_vars_n, domain_vars_d, var_n, var_d, cov_val]

  # 6. Identify domain columns for each side
  doms_num <- setdiff(colnames(stats_num), c("var", "estimate", "se"))
  doms_den <- setdiff(colnames(stats_den), c("var", "estimate", "se"))

  # 7. Add _n/_d suffixes to domain cols and rename var/estimate/se, then cross-join
  suffix_n <- "_n"
  suffix_d <- "_d"

  stats_num_suf <- stats_num
  if (length(doms_num) > 0) {
    stats_num_suf <- stats_num_suf %>%
      dplyr::rename_with(~ paste0(.x, suffix_n), dplyr::all_of(doms_num))
  }
  stats_num_suf <- stats_num_suf %>%
    dplyr::rename(var_n = var, estimate_n = estimate, se_n = se)

  stats_den_suf <- stats_den
  if (length(doms_den) > 0) {
    stats_den_suf <- stats_den_suf %>%
      dplyr::rename_with(~ paste0(.x, suffix_d), dplyr::all_of(doms_den))
  }
  stats_den_suf <- stats_den_suf %>%
    dplyr::rename(var_d = var, estimate_d = estimate, se_d = se)

  # Cross-join produces all (domain_n, var_n) x (domain_d, var_d) combinations
  pop_joined <- dplyr::cross_join(stats_num_suf, stats_den_suf)

  # 8. Add _n/_d suffixes to pop_cov_long domain cols to match pop_joined, then join
  pop_cov_long_suf <- pop_cov_long
  if (length(doms_num) > 0) {
    pop_cov_long_suf <- pop_cov_long_suf %>%
      dplyr::rename_with(~ paste0(.x, suffix_n), dplyr::all_of(doms_num))
  }
  if (length(doms_den) > 0) {
    pop_cov_long_suf <- pop_cov_long_suf %>%
      dplyr::rename_with(~ paste0(.x, suffix_d), dplyr::all_of(doms_den))
  }

  doms_num_suf <- if (length(doms_num) > 0) paste0(doms_num, suffix_n) else character(0)
  doms_den_suf <- if (length(doms_den) > 0) paste0(doms_den, suffix_d) else character(0)

  cov_join_keys <- c(doms_num_suf, doms_den_suf, "var_n", "var_d")
  pop_full <- dplyr::left_join(pop_joined, pop_cov_long_suf, by = cov_join_keys)
  # Missing cov_val means numerator and denominator never co-occur on the same plot,
  # so all cross-products y_n * y_d = 0 and the true covariance is 0.
  pop_full <- pop_full %>%
    dplyr::mutate(cov_val = dplyr::coalesce(cov_val, 0))

  # 9. Apply the ratio variance formula:
  #    v(R) = (1/Y_d^2) * [v(Y_n) + R^2*v(Y_d) - 2*R*cov(Y_n, Y_d)]
  all_doms <- c(doms_num_suf, doms_den_suf)

  final_res <- pop_full %>%
    dplyr::mutate(
      estimate = estimate_n / estimate_d,
      var_ratio = (1 / estimate_d^2) * (
        se_n^2 +
          (estimate_n / estimate_d)^2 * se_d^2 -
          2 * (estimate_n / estimate_d) * cov_val
      ),
      se = sqrt(pmax(var_ratio, 0))
    ) %>%
    dplyr::select(
      dplyr::all_of(all_doms),
      var_n,
      var_d,
      estimate,
      se
    )

  return(final_res)
})


# --- PSR internal helpers ---

#' Aggregate plot-level data for one side of the ratio
#' @noRd
.psr_aggregate <- function(handler, parsed) {
  if (parsed$slot == "tree") {
    if (length(parsed$targets) == 1 && parsed$targets == "1") {
      .make_tree_aggregates(handler, adjusted = TRUE, sparse = TRUE)
    } else {
      syms <- rlang::syms(parsed$targets)
      .make_tree_aggregates(handler, !!!syms, adjusted = TRUE, sparse = TRUE)
    }
  } else if (parsed$slot == "cond") {
    .make_cond_aggregates(handler, adjusted = TRUE, sparse = TRUE)
  } else {
    stop("Unsupported slot: ", parsed$slot)
  }
}

#' Resolve value column names from parsed formula
#' @noRd
.psr_val_cols <- function(parsed) {
  if (length(parsed$targets) == 1 && parsed$targets == "1") {
    if (parsed$slot == "cond") "prop" else "tree_count"
  } else {
    parsed$targets
  }
}

#' Run one side through the full post-stratification pipeline
#' @noRd
.psr_pop_estimate <- function(agg_data, handler, targets) {
  strata_data <- .ps_join_strata(agg_data, handler)
  strata_means <- .ps_strata_means(strata_data, targets)
  eu_data <- .ps_eu_estimates(strata_means, targets)
  .ps_pop_estimates(eu_data, handler, targets)
}
