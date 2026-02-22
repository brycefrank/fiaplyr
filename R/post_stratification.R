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
#' Variance formula: v_eu = (1/n) * sum_h [w_h*n_h + (1-w_h)*n_h/n] * v_h
#' (Eq 4.6/4.14 from BNP 2005, without A_T^2 since we estimate means not totals)
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
#' @return A lazy query with population-level estimates and standard errors.
#' @noRd
.ps_pop_stats <- function(eu_stats, handler, targets) {
  eu_weights <- handler@tables$pop_estn_unit %>%
    dplyr::mutate(w_eu = as.numeric(P1PNTCNT_EU) / sum(P1PNTCNT_EU, na.rm = TRUE)) %>%
    dplyr::select(CN, w_eu)

  mean_targets <- paste0(targets, "_mean")
  var_targets <- paste0(targets, "_var")
  se_targets <- paste0(targets, "_se")
  all_stat_cols <- c(mean_targets, var_targets)

  all_cols <- colnames(eu_stats)
  domain_vars <- setdiff(all_cols, c("ESTN_UNIT_CN", all_stat_cols))

  result <- eu_stats %>%
    dplyr::left_join(eu_weights, by = c("ESTN_UNIT_CN" = "CN")) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(domain_vars))) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(mean_targets),
        ~ sum(.x * w_eu, na.rm = TRUE)
      ),
      dplyr::across(
        dplyr::all_of(var_targets),
        ~ sum(.x * w_eu^2, na.rm = TRUE)
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
