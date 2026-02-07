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

  agg_num <- aggregate(object@numerator, f_num, sparse = TRUE)
  agg_den <- aggregate(object@denominator, f_den, sparse = TRUE)

  # 2. Identify Columns (Keys, Domains, Values)
  # -------------------------------------------
  plot_keys <- .plot_keys

  parsed_num <- parse_formula(f_num)
  parsed_den <- parse_formula(f_den)

  # Get actual column names from aggregated data
  cols_num <- colnames(agg_num)
  cols_den <- colnames(agg_den)

  # Target variables from formula (what we're aggregating)
  targets_num <- parsed_num$targets
  targets_den <- parsed_den$targets

  # Value columns: Find actual column names that match the targets
  # For "1", the aggregated column is "prop" (from cond aggregation)
  # For variable names, they appear as-is
  vals_num <- if (length(targets_num) == 1 && targets_num == "1") {
    "prop"
  } else {
    # Match target names to actual columns
    intersect(cols_num, targets_num)
  }
  
  vals_den <- if (length(targets_den) == 1 && targets_den == "1") {
    "prop"
  } else {
    intersect(cols_den, targets_den)
  }

  # Domain columns: everything else (excluding plot keys and values)
  doms_num <- setdiff(cols_num, c(plot_keys, vals_num))
  doms_den <- setdiff(cols_den, c(plot_keys, vals_den))

  # 3. Rename with Suffixes (Lazy)
  # ------------------------------
  suffix_num <- "_n"
  suffix_den <- "_d"

  # Helper to generate rename mapping
  get_rename_map <- function(vars, suffix) {
    setNames(vars, paste0(vars, suffix))
  }

  # Rename numerator columns
  map_dom_n <- get_rename_map(doms_num, suffix_num)
  map_val_n <- get_rename_map(vals_num, suffix_num)
  agg_num_renamed <- agg_num %>%
    dplyr::rename(!!!map_dom_n, !!!map_val_n)

  # Rename denominator columns
  map_dom_d <- get_rename_map(doms_den, suffix_den)
  map_val_d <- get_rename_map(vals_den, suffix_den)
  agg_den_renamed <- agg_den %>%
    dplyr::rename(!!!map_dom_d, !!!map_val_d)

  # Update lists of names with suffixes
  doms_num_suf <- names(map_dom_n)
  vals_num_suf <- names(map_val_n)
  doms_den_suf <- names(map_dom_d)
  vals_den_suf <- names(map_val_d)

  all_doms <- c(doms_num_suf, doms_den_suf)
  all_vals <- c(vals_num_suf, vals_den_suf)

  joined <- dplyr::full_join(agg_num_renamed, agg_den_renamed, by = plot_keys)

  # Fill NAs with 0 using coalesce
  # mutate(col = coalesce(col, 0))
  mutate_args <- lapply(all_vals, function(v) {
    rlang::expr(dplyr::coalesce(!!rlang::sym(v), 0))
  })
  names(mutate_args) <- all_vals

  joined_filled <- joined %>%
    dplyr::mutate(!!!mutate_args)

  final_dat <- .ps_join_strata(joined_filled, object@numerator)

  # Estimate Totals (Means) - Lazy Aggregation
  group_cols_strat <- c("ESTN_UNIT_CN", "STRATUM_CN", "w_h", "n_h", "n", all_doms)

  strat_sums <- final_dat %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols_strat))) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(all_vals), sum, .names = "sum_{.col}"),
      n_observed = dplyr::n(),
      n_missing = dplyr::first(n_h) - dplyr::n(),
      .groups = "drop"
    )

  # Calculate means from sums
  mean_args <- lapply(all_vals, function(v) {
    rlang::expr(!!rlang::sym(paste0("sum_", v)) / n_h)
  })
  names(mean_args) <- all_vals

  strat_means <- strat_sums %>%
    dplyr::mutate(!!!mean_args) %>%
    dplyr::select(-dplyr::starts_with("sum_"))


  group_cols_eu <- c("ESTN_UNIT_CN", all_doms)

  eu_means <- strat_means %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols_eu))) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(all_vals), ~ sum(.x * w_h, na.rm = TRUE)),
      n_observed = sum(n_observed, na.rm = TRUE),
      n_missing = sum(n_missing, na.rm = TRUE),
      .groups = "drop"
    )

  # Weighted Sum across EstUnits
  eu_weights <- object@numerator@tables$pop_estn_unit %>%
    dplyr::mutate(w_eu = P1PNTCNT_EU / sum(P1PNTCNT_EU, na.rm = TRUE)) %>%
    dplyr::select(ESTN_UNIT_CN = CN, w_eu)

  pop_est <- eu_means %>%
    dplyr::left_join(eu_weights, by = "ESTN_UNIT_CN") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(all_doms))) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(all_vals), ~ sum(.x * w_eu, na.rm = TRUE)),
      n_observed = sum(n_observed, na.rm = TRUE),
      n_missing = sum(n_missing, na.rm = TRUE),
      .groups = "drop"
    )

  # 7. Reshape and Compute Ratios
  # ------------------------------------
  # Collect the aggregated data since it's now small and we need to do
  # string manipulation (substr) which doesn't translate well to SQL
  pop_est_local <- pop_est %>% dplyr::collect()

  # Compute suffix lengths as regular R values
  n_suffix_len <- nchar(suffix_num)
  d_suffix_len <- nchar(suffix_den)

  # Pivot Numerator
  long_n <- pop_est_local %>%
    dplyr::select(dplyr::all_of(c(all_doms, vals_num_suf, "n_observed", "n_missing"))) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(vals_num_suf),
      names_to = "var_n_raw",
      values_to = "val_n"
    ) %>%
    dplyr::mutate(
      var_n = substr(var_n_raw, 1, nchar(var_n_raw) - n_suffix_len)
    )

  # Pivot Denominator
  long_d <- pop_est_local %>%
    dplyr::select(dplyr::all_of(c(all_doms, vals_den_suf, "n_observed", "n_missing"))) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(vals_den_suf),
      names_to = "var_d_raw",
      values_to = "val_d"
    ) %>%
    dplyr::mutate(
      var_d = substr(var_d_raw, 1, nchar(var_d_raw) - d_suffix_len)
    )

  # Join and Estimate
  final_res <- long_n %>%
    dplyr::inner_join(long_d, by = c(all_doms, "n_observed", "n_missing")) %>%
    dplyr::mutate(
      estimate = val_n / val_d
    ) %>%
    dplyr::select(
      dplyr::all_of(all_doms),
      var_n,
      var_d,
      estimate,
      n_observed,
      n_missing
    )

  return(final_res)
})
