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
    dplyr::mutate(w_eu = P1PNTCNT_EU / sum(P1PNTCNT_EU, na.rm = TRUE)) %>%
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
