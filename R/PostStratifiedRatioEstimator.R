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
  # Note: This returns a lazy query or local df depending on input.
  # We store it as is.
  strata_weights <- numerator@tables$pop_stratum %>%
    dplyr::inner_join(
      numerator@tables$pop_estn_unit,
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
  if (length(args) != 2) stop("Must provide exactly two formulas (numerator, denominator).")

  f_num <- args[[1]]
  f_den <- args[[2]]

  agg_num <- aggregate(object@numerator, f_num, sparse = FALSE)
  agg_den <- aggregate(object@denominator, f_den, sparse = FALSE)

  # 2. Identify Columns (Keys, Domains, Values)
  # -------------------------------------------
  plot_keys <- c("PLT_CN", "STATECD", "INVYR", "PLOT", "COUNTYCD")

  parsed_num <- parse_formula(f_num)
  parsed_den <- parse_formula(f_den)

  vals_num <- parsed_num$targets
  vals_den <- parsed_den$targets

  if (length(vals_num) == 1 && vals_num == "1") vals_num <- "1"
  if (length(vals_den) == 1 && vals_den == "1") vals_den <- "1"

  # Use colnames() on lazy objects to inspect schema without pulling data
  cols_num <- colnames(agg_num)
  cols_den <- colnames(agg_den)

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

  strata_weights <- object@numerator@tables$pop_stratum %>%
    dplyr::inner_join(
      object@numerator@tables$pop_estn_unit,
      by = c("ESTN_UNIT_CN" = "CN"),
      suffix = c("", ".eu")
    ) %>%
    dplyr::mutate(
      w_h = P1POINTCNT / P1PNTCNT_EU # Keep as DB numeric type
    ) %>%
    dplyr::select(
      STRATUM_CN = CN, ESTN_UNIT_CN, w_h, P2POINTCNT, AREA_USED
    )

  plot_stratum <- object@numerator@tables$pop_plot_stratum_assgn %>%
    dplyr::select(PLT_CN, STRATUM_CN)

  final_dat <- joined_filled %>%
    dplyr::inner_join(plot_stratum, by = "PLT_CN") %>%
    dplyr::inner_join(strata_weights, by = "STRATUM_CN")

  # Estimate Totals (Means) - Lazy Aggregation
  group_cols_strat <- c("ESTN_UNIT_CN", "STRATUM_CN", "w_h", "P2POINTCNT", all_doms)

  strat_sums <- final_dat %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols_strat))) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(all_vals), sum, .names = "sum_{.col}"),
      .groups = "drop"
    )

  # Calculate means from sums
  # val_mean = sum_val / P2POINTCNT
  mean_args <- lapply(all_vals, function(v) {
    rlang::expr(!!rlang::sym(paste0("sum_", v)) / P2POINTCNT)
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
      .groups = "drop"
    )

  # 7. Collect and Compute Ratios (Local)
  # -------------------------------------
  # At this point, the data is aggregated to (Domains x 1)
  # It is small enough to collect.
  # Iterate over all pairs of Numerator x Denominator variables
  results <- list()

  for (n_col in vals_num_suf) {
    for (d_col in vals_den_suf) {
      # Remove suffix for clean variable name
      n_name <- substr(n_col, 1, nchar(n_col) - nchar(suffix_num))
      d_name <- substr(d_col, 1, nchar(d_col) - nchar(suffix_den))

      tmp_res <- pop_est %>%
        dplyr::mutate(
          estimate = .data[[n_col]] / .data[[d_col]],
          variable = n_name,
          den_variable = d_name
        ) %>%
        dplyr::select(
          dplyr::all_of(all_doms),
          variable,
          den_variable,
          estimate
        )

      results[[paste(n_col, d_col)]] <- tmp_res
    }
  }

  final_res <- dplyr::bind_rows(results)

  return(final_res)
})
