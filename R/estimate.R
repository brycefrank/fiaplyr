#' Get Strata Summary
#'
#' Calculates the strata summary from the population tables.
#'
#' @param object A EvalHandler object.
#' @return A dataframe with strata summary statistics.
#' @keywords internal
.get_strata_summary <- function(object) {
  object@pop_stratum %>%
    dplyr::inner_join(
      object@pop_estn_unit,
      by = c("ESTN_UNIT_CN" = "CN"),
      suffix = c("", ".eu")
    ) %>%
    dplyr::group_by(
      ESTN_UNIT_CN
    ) %>%
    dplyr::mutate(
      n = sum(P2POINTCNT, na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      w_h = as.numeric(P1POINTCNT) / P1PNTCNT_EU, n_h = P2POINTCNT
    ) %>%
    dplyr::select(
      STRATUM_CN = CN, ESTN_UNIT_CN, w_h, n_h, n
    )
}

#' Get Subplot Type Adjustment Factors
#'
#' @param object A EvalHandler object.
#' @param subptypes A character vector of subplot types
#' @keywords internal
.get_subptype_adjustment_factors <- function(object, subptypes = c("MACR", "SUBP", "MICR")) {
  subptype_vars <- paste0("ADJ_FACTOR_", subptypes)

  object@pop_stratum %>%
    dplyr::select(
      CN,
      dplyr::all_of(subptype_vars)
    ) |>
    dplyr::rename(
      STRATUM_CN = CN
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(subptype_vars),
      names_to = "SUBPTYPE",
      names_prefix = "ADJ_FACTOR_",
      values_to = "ADJ_FACTOR"
    )
}

#' Estimate condition variables by stratum
#'
#' @param object A BaseHandler object.
#' @keywords internal
.estimate_cond_strata <- function(object) {
  cond_values <- .make_cond_aggregates(object, adjusted = TRUE)
  strata_summary <- .get_strata_summary(object)

  combined_data <- cond_values %>%
    dplyr::inner_join(
      object@pop_plot_stratum_assgn %>% dplyr::select(PLT_CN, STRATUM_CN),
      by = "PLT_CN"
    ) |>
    dplyr::inner_join(strata_summary, by = "STRATUM_CN")

  strat_keys <- c("STRATUM_CN", "ESTN_UNIT_CN", "EVAL_CN", "w_h", "n_h", "n")
  plot_keys <- c("PLT_CN", "STATECD", "INVYR", "PLOT", "COUNTYCD")

  # All columns in combined_data
  all_cols <- colnames(combined_data)
  group_vars <- setdiff(all_cols, c(plot_keys, strat_keys, "prop"))

  stratum_stats <- combined_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("ESTN_UNIT_CN", "STRATUM_CN", "w_h", "n_h", "n", group_vars)))) %>%
    dplyr::summarise(
      p_strat = sum(prop, na.rm = TRUE) / n_h
    ) %>%
    dplyr::ungroup()

  return(stratum_stats)
}

.estimate_cond_eu <- function(object) {
  strata_stats <- .estimate_cond_strata(object) |>
    dplyr::left_join(
      object@pop_estn_unit |> dplyr::select(CN),
      by = c("ESTN_UNIT_CN" = "CN")
    )

  strat_keys <- c("STRATUM_CN", "ESTN_UNIT_CN", "EVAL_CN", "w_h", "n_h", "n")
  plot_keys <- c("PLT_CN", "STATECD", "INVYR", "PLOT", "COUNTYCD")

  # All columns in combined_data
  all_cols <- colnames(strata_stats)
  group_vars <- setdiff(all_cols, c(plot_keys, strat_keys, "p_strat"))

  strata_stats |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("ESTN_UNIT_CN", group_vars)))) |>
    dplyr::summarise(
      p_eu = sum(w_h * p_strat, na.rm = TRUE)
    ) |>
    dplyr::ungroup()
}

.estimate_cond <- function(object) {
  # Calculate eu weights
  eu_weights <- object@pop_estn_unit |>
    dplyr::mutate(
      w_eu = P1PNTCNT_EU / sum(P1PNTCNT_EU, na.rm = TRUE)
    ) |>
    dplyr::select(CN, w_eu)


  eu_stats <- .estimate_cond_eu(object)

  eu_keys <- c("ESTN_UNIT_CN")

  # All columns in combined_data
  all_cols <- colnames(eu_stats)
  group_vars <- setdiff(all_cols, c(eu_keys, "p_eu", "w_eu"))

  eu_stats |>
    dplyr::left_join(eu_weights, by = c("ESTN_UNIT_CN" = "CN")) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(eu_keys, group_vars)))) |>
    dplyr::summarise(
      p = sum(p_eu * w_eu, na.rm = TRUE)
    ) |>
    dplyr::ungroup()
}

.estimate_tree_strata <- function(object, ...) {
  plot_values <- .make_tree_aggregates(object, ..., level = "plot")

  strata_summary <- .get_strata_summary(object)

  combined_data <- plot_values %>%
    dplyr::inner_join(
      object@pop_plot_stratum_assgn %>% dplyr::select(PLT_CN, STRATUM_CN),
      by = "PLT_CN"
    ) %>%
    dplyr::inner_join(strata_summary, by = c("STRATUM_CN"))

  plot_keys <- c("PLT_CN", "STATECD", "INVYR", "PLOT", "COUNTYCD")
  strat_keys <- c("STRATUM_CN", "ESTN_UNIT_CN", "EVAL_CN", "w_h", "n_h", "n")

  # All columns in combined_data
  all_cols <- colnames(combined_data)
  target_vars <- names(tidyselect::eval_select(rlang::expr(c(...)), combined_data))

  group_vars <- setdiff(all_cols, c(plot_keys, strat_keys, target_vars))

  if (length(target_vars) == 0) {
    stop("No target tree variables specified.")
  }

  # Calculate Stratum Means
  strata_stats <- combined_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("ESTN_UNIT_CN", "STRATUM_CN", "w_h", "n_h", "n", group_vars)))) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(target_vars),
        list(
          mean = ~ sum(.x, na.rm = TRUE) / n
        ),
        .names = "{.col}_{.fn}"
      )
    ) %>%
    dplyr::ungroup()

  return(strata_stats)
}
