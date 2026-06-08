# Shared post-stratification helper functions used by both
# PostStratifiedEstimator and PostStratifiedRatioEstimator.

#' Join plot-level data to strata assignment and summary
#'
#' @param plot_data A lazy query with plot-level data (must have PLT_CN).
#' @param handler A EvalHandler object.
#' @return A lazy query with strata columns (STRATUM_CN, ESTN_UNIT_CN, w_h, n_h, n) added.
#' @noRd
.ps_join_strata <- function(plot_data, handler) {
  strata_summary <- .get_strata_summary(handler)
  plot_data %>%
    dplyr::inner_join(
      handler@tables$pop_plot_stratum_assgn %>% dplyr::select(PLT_CN, STRATUM_CN),
      by = "PLT_CN"
    ) %>%
    dplyr::inner_join(strata_summary, by = "STRATUM_CN")
}

#' Compute stratum means for target variables
#'
#' Groups by strata and domain variables, then computes the mean of each
#' target variable within each stratum as sum(x) / n_h.
#'
#' @param strata_data Data joined to strata (output of .ps_join_strata).
#' @param targets Character vector of target column names to aggregate.
#' @return A lazy query with stratum-level means.
#' @noRd
.ps_strata_means <- function(strata_data, targets) {
  all_cols <- colnames(strata_data)
  domain_vars <- setdiff(all_cols, c(.plot_keys, .strat_keys, targets))

  strata_data %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(c("ESTN_UNIT_CN", "STRATUM_CN", "w_h", "n_h", "n", domain_vars))
      )
    ) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(targets), ~ sum(.x, na.rm = TRUE) / n_h)
    ) %>%
    dplyr::ungroup()
}

#' Roll up stratum means to estimation unit level
#'
#' Weighted sum of stratum means by w_h within each estimation unit.
#'
#' @param strata_means Stratum-level means (output of .ps_strata_means).
#' @param targets Character vector of target column names.
#' @return A lazy query with estimation-unit-level estimates.
#' @noRd
.ps_eu_estimates <- function(strata_means, targets) {
  all_cols <- colnames(strata_means)
  domain_vars <- setdiff(all_cols, c(.strat_keys, targets))

  strata_means %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("ESTN_UNIT_CN", domain_vars)))) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(targets), ~ sum(w_h * .x, na.rm = TRUE))
    ) %>%
    dplyr::ungroup()
}

#' Roll up estimation unit estimates to population total
#'
#' Computes estimation unit weights (w_eu) and takes the weighted sum
#' across estimation units.
#'
#' @param eu_data Estimation-unit-level data (output of .ps_eu_estimates).
#' @param handler A EvalHandler object.
#' @param targets Character vector of target column names.
#' @return A lazy query with population-level estimates.
#' @noRd
.ps_pop_estimates <- function(eu_data, handler, targets) {
  eu_weights <- handler@tables$pop_estn_unit %>%
    dplyr::mutate(w_eu = as.numeric(P1PNTCNT_EU) / sum(P1PNTCNT_EU, na.rm = TRUE)) %>%
    dplyr::select(CN, w_eu)

  all_cols <- colnames(eu_data)
  domain_vars <- setdiff(all_cols, c("ESTN_UNIT_CN", targets))

  eu_data %>%
    dplyr::left_join(eu_weights, by = c("ESTN_UNIT_CN" = "CN")) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(domain_vars))) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(targets), ~ sum(.x * w_eu, na.rm = TRUE))
    ) %>%
    dplyr::ungroup()
}

# --- Variance-aware pipeline functions (PostStratifiedEstimator) ---

#' Compute stratum means and variances for target variables
#'
#' Groups by strata and domain variables, then computes the mean and variance
#' of each target variable within each stratum. Handles sparse data correctly:
#' sum and sum_sq only need observed (non-zero) rows because 0^2 = 0.
#'
#' @param strata_data Data joined to strata (output of .ps_join_strata).
#' @param targets Character vector of target column names to aggregate.
#' @return A lazy query with stratum-level means and variances.
#' @noRd
.ps_strata_stats <- function(strata_data, targets) {
  all_cols <- colnames(strata_data)
  domain_vars <- setdiff(all_cols, c(.plot_keys, .strat_keys, targets))
  var_targets <- paste0(targets, "_var")

  strata_data %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(c("ESTN_UNIT_CN", "STRATUM_CN", "w_h", "n_h", "n", domain_vars))
      )
    ) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(targets),
        list(
          mean = ~ sum(.x, na.rm = TRUE) / n_h,
          var = ~ dplyr::case_when(
            n_h <= 1 ~ 0,
            TRUE ~ (sum(.x^2, na.rm = TRUE) - n_h * (sum(.x, na.rm = TRUE) / n_h)^2) /
              (n_h * (n_h - 1))
          )
        ),
        .names = "{.col}_{.fn}"
      )
    ) %>%
    dplyr::ungroup()
}

#' Roll up stratum stats to estimation unit level
#'
#' Computes weighted mean and variance at the estimation unit level.
#' Variance formula: \eqn{v_eu = (1/n) * sum_h [w_h*n_h + (1-w_h)*n_h/n] * v_h}
#' (Eq 4.6/4.14 from BNP 2005, without \eqn{A_T^2} since we estimate means not totals)
#'
#' @param strata_stats Stratum-level stats (output of .ps_strata_stats).
#' @param targets Character vector of original target column names.
#' @return A lazy query with estimation-unit-level means and variances.
#' @noRd
.ps_eu_stats <- function(strata_stats, targets) {
  mean_targets <- paste0(targets, "_mean")
  var_targets <- paste0(targets, "_var")
  all_stat_cols <- c(mean_targets, var_targets)

  all_cols <- colnames(strata_stats)
  domain_vars <- setdiff(all_cols, c(.strat_keys, all_stat_cols))

  strata_stats %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("ESTN_UNIT_CN", domain_vars)))) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(mean_targets),
        ~ sum(w_h * .x, na.rm = TRUE)
      ),
      dplyr::across(
        dplyr::all_of(var_targets),
        ~ sum((1 / n) * (w_h * n_h + (1 - w_h) * n_h / n) * .x, na.rm = TRUE)
      )
    ) %>%
    dplyr::ungroup()
}

#' Roll up estimation unit stats to population level
#'
#' Computes EU weights and takes weighted sum for means and weighted-squared
#' sum for variances (EUs are independent). Returns final estimate and SE.
#'
#' @param eu_stats Estimation-unit-level stats (output of .ps_eu_stats).
#' @param handler A EvalHandler object.
#' @param targets Character vector of original target column names.
#' @param output Output scale: "mean" (default) or "total".
#' @return A lazy query with population-level estimates and standard errors.
#' @noRd
.ps_pop_stats <- function(eu_stats, handler, targets, output = "mean") {
  output <- match.arg(output, c("mean", "total"))

  eu_weights <- handler@tables$pop_estn_unit %>%
    dplyr::mutate(
      w_eu = as.numeric(P1PNTCNT_EU) / sum(P1PNTCNT_EU, na.rm = TRUE),
      eu_area = as.numeric(AREA_USED)
    ) %>%
    dplyr::select(CN, w_eu, eu_area)

  mean_targets <- paste0(targets, "_mean")
  var_targets <- paste0(targets, "_var")
  se_targets <- paste0(targets, "_se")
  all_stat_cols <- c(mean_targets, var_targets)

  all_cols <- colnames(eu_stats)
  domain_vars <- setdiff(all_cols, c("ESTN_UNIT_CN", all_stat_cols))

  result <- eu_stats %>%
    dplyr::left_join(eu_weights, by = c("ESTN_UNIT_CN" = "CN")) %>%
    dplyr::mutate(
      coeff_mean = dplyr::if_else(output == "mean", w_eu, eu_area),
      coeff_var = dplyr::if_else(output == "mean", w_eu^2, eu_area^2)
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(domain_vars))) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(mean_targets),
        ~ sum(.x * coeff_mean, na.rm = TRUE)
      ),
      dplyr::across(
        dplyr::all_of(var_targets),
        ~ sum(.x * coeff_var, na.rm = TRUE)
      )
    ) %>%
    dplyr::ungroup()

  result <- result %>% dplyr::collect()

  mean_long <- result %>%
    dplyr::select(dplyr::all_of(c(domain_vars, mean_targets))) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(mean_targets),
      names_to = "var_raw",
      values_to = "estimate"
    ) %>%
    dplyr::mutate(var = sub("_mean$", "", var_raw)) %>%
    dplyr::select(-var_raw)

  var_long <- result %>%
    dplyr::select(dplyr::all_of(c(domain_vars, var_targets))) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(var_targets),
      names_to = "var_raw",
      values_to = "var_val"
    ) %>%
    dplyr::mutate(var = sub("_var$", "", var_raw)) %>%
    dplyr::select(-var_raw)

  if (length(domain_vars) == 0) {
    joined_long <- dplyr::inner_join(mean_long, var_long, by = "var")
  } else {
    joined_long <- dplyr::inner_join(mean_long, var_long, by = c(domain_vars, "var"))
  }

  final_res <- joined_long %>%
    dplyr::mutate(se = sqrt(var_val)) %>%
    dplyr::select(dplyr::all_of(domain_vars), var, estimate, se)

  final_res
}


# --- Ratio estimator covariance pipeline ---

#' Compute stratum-level sample covariance between numerator and denominator variables
#'
#' Uses the computational formula for sample covariance within each stratum.
#' The inner join on PLT_CN is correct: plots absent from one side have zero
#' values that contribute 0 to the cross-product sum.
#'
#' Domain variables shared between both sides are joined on (avoiding duplication).
#' Domain variables unique to one side produce covariances for all combinations
#' of (numerator domain, denominator domain) within each stratum.
#'
#' @param strata_data_num Strata-joined numerator data (output of .ps_join_strata).
#' @param strata_data_den Strata-joined denominator data (output of .ps_join_strata).
#' @param targets_num Character vector of numerator target column names.
#' @param targets_den Character vector of denominator target column names.
#' @param cov_cols Character vector of output column names
#' @return A lazy query with stratum-level covariance columns.
#' @noRd
.ps_strata_cov <- function(strata_data_num, strata_data_den, targets_num, targets_den, cov_cols) {
  all_cols_n <- colnames(strata_data_num)
  all_cols_d <- colnames(strata_data_den)
  domain_vars_n <- setdiff(all_cols_n, c(.plot_keys, .strat_keys, targets_num))
  domain_vars_d <- setdiff(all_cols_d, c(.plot_keys, .strat_keys, targets_den))
  shared_domain <- intersect(domain_vars_n, domain_vars_d)
  all_domain_vars <- union(domain_vars_n, domain_vars_d)

  # Suffix denominator targets to avoid column-name collision after joining
  targets_den_suf <- paste0(targets_den, ".__den")
  strata_d_slim <- strata_data_den %>%
    dplyr::select(dplyr::all_of(c("PLT_CN", domain_vars_d, targets_den))) %>%
    dplyr::rename_with(~ paste0(.x, ".__den"), dplyr::all_of(targets_den))

  # Inner join: PLT_CN plus any domain variables shared between both sides
  join_keys <- c("PLT_CN", shared_domain)
  joined <- strata_data_num %>%
    dplyr::inner_join(strata_d_slim, by = join_keys)

  group_cols <- c("ESTN_UNIT_CN", "STRATUM_CN", "w_h", "n_h", "n", all_domain_vars)

  # Build one summarise expression per (targets_num[i], targets_den[j]) pair.
  # cov_h = (sum(y_n * y_d) - sum(y_n)*sum(y_d)/n_h) / (n_h*(n_h-1))
  cov_exprs <- list()
  k <- 1L
  for (i in seq_along(targets_num)) {
    for (j in seq_along(targets_den)) {
      tn <- rlang::sym(targets_num[i])
      td <- rlang::sym(targets_den_suf[j])
      cov_exprs[[cov_cols[k]]] <- rlang::expr(
        dplyr::case_when(
          n_h <= 1 ~ 0,
          TRUE ~ (sum(!!tn * !!td, na.rm = TRUE) -
                    sum(!!tn, na.rm = TRUE) * sum(!!td, na.rm = TRUE) / n_h) /
            (n_h * (n_h - 1))
        )
      )
      k <- k + 1L
    }
  }

  joined %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(!!!cov_exprs) %>%
    dplyr::ungroup()
}

#' Roll up stratum covariances to estimation unit level
#'
#' Uses the same BNP 2005 Eq 4.6/4.14 weighting as .ps_eu_stats.
#'
#' @param strata_cov Stratum-level covariances (output of .ps_strata_cov).
#' @param cov_cols Character vector of covariance column names.
#' @return A lazy query with estimation-unit-level covariances.
#' @noRd
.ps_eu_cov <- function(strata_cov, cov_cols) {
  all_cols <- colnames(strata_cov)
  domain_vars <- setdiff(all_cols, c(.strat_keys, cov_cols))

  strata_cov %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("ESTN_UNIT_CN", domain_vars)))) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(cov_cols),
        ~ sum((1 / n) * (w_h * n_h + (1 - w_h) * n_h / n) * .x, na.rm = TRUE)
      )
    ) %>%
    dplyr::ungroup()
}

#' Roll up estimation unit covariances to population level
#'
#' Uses w_eu^2 coefficients (mean scale), consistent with .ps_pop_stats(output="mean").
#' EUs are independent, so population covariance is a weighted sum of EU covariances.
#'
#' @param eu_cov EU-level covariances (output of .ps_eu_cov).
#' @param handler A EvalHandler object (used to fetch EU weights).
#' @param cov_cols Character vector of covariance column names.
#' @return A collected tibble with population-level covariances.
#' @noRd
.ps_pop_cov <- function(eu_cov, handler, cov_cols) {
  eu_weights <- handler@tables$pop_estn_unit %>%
    dplyr::mutate(w_eu = as.numeric(P1PNTCNT_EU) / sum(P1PNTCNT_EU, na.rm = TRUE)) %>%
    dplyr::select(CN, w_eu)

  all_cols <- colnames(eu_cov)
  domain_vars <- setdiff(all_cols, c("ESTN_UNIT_CN", cov_cols))

  eu_cov %>%
    dplyr::left_join(eu_weights, by = c("ESTN_UNIT_CN" = "CN")) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(domain_vars))) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(cov_cols),
        ~ sum(.x * w_eu^2, na.rm = TRUE)
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::collect()
}
