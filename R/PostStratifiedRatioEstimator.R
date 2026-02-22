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

  # 1. Parse formulas and aggregate each side independently
  parsed_num <- parse_formula(f_num)
  parsed_den <- parse_formula(f_den)

  agg_num <- .psr_aggregate(object@numerator, parsed_num)
  agg_den <- .psr_aggregate(object@denominator, parsed_den)

  # 2. Resolve value column names
  vals_num <- .psr_val_cols(parsed_num)
  vals_den <- .psr_val_cols(parsed_den)

  # 3. Run each through the independent strata/EU/pop pipeline
  pop_num <- .psr_pop_estimate(agg_num, object@numerator, vals_num)
  pop_den <- .psr_pop_estimate(agg_den, object@denominator, vals_den)

  # 4. Identify domain columns on each side
  doms_num <- setdiff(colnames(pop_num), vals_num)
  doms_den <- setdiff(colnames(pop_den), vals_den)

  # 5. Rename with suffixes to avoid collisions
  suffix_num <- "_n"
  suffix_den <- "_d"

  rename_map <- function(vars, suffix) {
    if (length(vars) == 0) {
      return(character(0))
    }
    setNames(vars, paste0(vars, suffix))
  }

  pop_num <- pop_num %>%
    dplyr::rename(!!!rename_map(doms_num, suffix_num), !!!rename_map(vals_num, suffix_num))
  pop_den <- pop_den %>%
    dplyr::rename(!!!rename_map(doms_den, suffix_den), !!!rename_map(vals_den, suffix_den))

  add_suffix <- function(vars, suffix) {
    if (length(vars) == 0) {
      return(character(0))
    }
    paste0(vars, suffix)
  }

  doms_num_suf <- add_suffix(doms_num, suffix_num)
  doms_den_suf <- add_suffix(doms_den, suffix_den)
  vals_num_suf <- add_suffix(vals_num, suffix_num)
  vals_den_suf <- add_suffix(vals_den, suffix_den)

  # 6. Cross-join and collect (results are small at pop level)
  pop_joined <- pop_num %>%
    dplyr::cross_join(pop_den) %>%
    dplyr::collect()

  # 7. Pivot to long for var_n x var_d cross product, compute ratios
  all_doms <- c(doms_num_suf, doms_den_suf)
  n_suffix_len <- nchar(suffix_num)
  d_suffix_len <- nchar(suffix_den)

  long_n <- pop_joined %>%
    dplyr::select(dplyr::all_of(c(all_doms, vals_num_suf))) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(vals_num_suf),
      names_to = "var_n_raw",
      values_to = "val_n"
    ) %>%
    dplyr::mutate(
      var_n = substr(var_n_raw, 1, nchar(var_n_raw) - n_suffix_len)
    )

  long_d <- pop_joined %>%
    dplyr::select(dplyr::all_of(c(all_doms, vals_den_suf))) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(vals_den_suf),
      names_to = "var_d_raw",
      values_to = "val_d"
    ) %>%
    dplyr::mutate(
      var_d = substr(var_d_raw, 1, nchar(var_d_raw) - d_suffix_len)
    )

  if (length(all_doms) == 0) {
    joined_long <- dplyr::cross_join(long_n, long_d)
  } else {
    joined_long <- dplyr::inner_join(long_n, long_d, by = all_doms)
  }

  final_res <- joined_long %>%
    dplyr::mutate(
      estimate = val_n / val_d
    ) %>%
    dplyr::select(
      dplyr::all_of(all_doms),
      var_n,
      var_d,
      estimate
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
