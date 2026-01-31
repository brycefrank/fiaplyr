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

  # 1. Aggregate Numerator and Denominator (Dense)
  # ----------------------------------------------
  # We use sparse = FALSE to ensure every plot has a row for every domain,
  # enabling a correct cross-product join.
  agg_num_lazy <- aggregate(object@numerator, f_num, sparse = FALSE)
  agg_den_lazy <- aggregate(object@denominator, f_den, sparse = FALSE)

  dat_num <- dplyr::collect(agg_num_lazy)
  dat_den <- dplyr::collect(agg_den_lazy)

  # 2. Identify Columns (Keys, Domains, Values)
  # -------------------------------------------
  plot_keys <- c("PLT_CN", "STATECD", "INVYR", "PLOT", "COUNTYCD")

  parsed_num <- parse_formula(f_num)
  parsed_den <- parse_formula(f_den)

  vals_num <- parsed_num$targets
  vals_den <- parsed_den$targets

  if (length(vals_num) == 1 && vals_num == "1") vals_num <- "1"
  if (length(vals_den) == 1 && vals_den == "1") vals_den <- "1"

  # Identify domains
  doms_num <- setdiff(colnames(dat_num), c(plot_keys, vals_num))
  doms_den <- setdiff(colnames(dat_den), c(plot_keys, vals_den))

  # 3. Rename with Suffixes
  # -----------------------
  # Suffixes _n and _d ensure we distinguish numerator/denominator domains and values
  suffix_num <- "_n"
  suffix_den <- "_d"

  # Helper to rename vector of columns
  rename_cols <- function(df, old_names, suffix) {
    name_map <- setNames(paste0(old_names, suffix), old_names)
    dplyr::rename(df, !!!name_map)
  }

  dat_num_renamed <- dat_num %>%
    rename_cols(doms_num, suffix_num) %>%
    rename_cols(vals_num, suffix_num)

  dat_den_renamed <- dat_den %>%
    rename_cols(doms_den, suffix_den) %>%
    rename_cols(vals_den, suffix_den)

  # Update lists of names
  doms_num_suf <- paste0(doms_num, suffix_num)
  vals_num_suf <- paste0(vals_num, suffix_num)
  doms_den_suf <- paste0(doms_den, suffix_den)
  vals_den_suf <- paste0(vals_den, suffix_den)

  all_doms <- c(doms_num_suf, doms_den_suf)
  all_vals <- c(vals_num_suf, vals_den_suf)

  # 4. Join and Fill Zeros
  # ----------------------
  # Full join on plot keys creates the Cartesian product of domains for each plot
  joined <- dplyr::full_join(dat_num_renamed, dat_den_renamed, by = plot_keys)

  # Fill NAs in value columns with 0
  joined <- joined %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(all_vals), ~ tidyr::replace_na(., 0)))

  # 5. Attach Weights
  # -----------------
  plot_stratum <- object@numerator@tables$pop_plot_stratum_assgn %>%
    dplyr::collect() %>%
    dplyr::select(PLT_CN, STRATUM_CN)

  strata_weights <- dplyr::collect(object@strata_weights)

  final_dat <- joined %>%
    dplyr::inner_join(plot_stratum, by = "PLT_CN") %>%
    dplyr::inner_join(strata_weights, by = "STRATUM_CN")

  # 6. Estimate Totals (Means)
  # --------------------------
  # Group by Estimation Unit, Stratum, and ALL Domain Variables
  group_cols_strat <- c("ESTN_UNIT_CN", "STRATUM_CN", "w_h", "P2POINTCNT", all_doms)

  strat_means <- final_dat %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols_strat))) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(all_vals), ~ sum(.x) / unique(P2POINTCNT)),
      .groups = "drop"
    )

  # Weighted Sum to Estn Unit
  group_cols_eu <- c("ESTN_UNIT_CN", all_doms)

  eu_means <- strat_means %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols_eu))) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(all_vals), ~ sum(.x * w_h)),
      .groups = "drop"
    )

  # Weighted Sum to Population
  eu_weights <- object@numerator@tables$pop_estn_unit %>%
    dplyr::collect() %>%
    dplyr::mutate(w_eu = P1PNTCNT_EU / sum(P1PNTCNT_EU, na.rm = TRUE)) %>%
    dplyr::select(ESTN_UNIT_CN = CN, w_eu)

  pop_est <- eu_means %>%
    dplyr::left_join(eu_weights, by = "ESTN_UNIT_CN") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(all_doms))) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(all_vals), ~ sum(.x * w_eu, na.rm = TRUE)),
      .groups = "drop"
    )

  # 7. Compute Ratios
  # -----------------
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
