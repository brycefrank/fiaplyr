#' Estimate Tree Means
#'
#' @param handler A EvalHandler object.
#' @param targets A character vector of target variables.
#' @keywords internal
estimate_tree_means <- function(handler, targets) {
  # Calculate eu weights
  eu_weights <- handler@pop_estn_unit |>
    dplyr::mutate(
      w_eu = P1PNTCNT_EU / sum(P1PNTCNT_EU, na.rm = TRUE)
    ) |>
    dplyr::select(CN, w_eu)

  eu_stats <- .estimate_tree_means_eu(handler, targets)

  eu_keys <- c("ESTN_UNIT_CN")

  all_cols <- colnames(eu_stats)
  group_vars <- setdiff(all_cols, c(eu_keys, targets, "w_eu"))

  eu_stats |>
    dplyr::left_join(eu_weights, by = c("ESTN_UNIT_CN" = "CN")) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(targets), ~ sum(.x * w_eu, na.rm = TRUE))
    ) |>
    dplyr::ungroup()
}

.estimate_tree_means_eu <- function(handler, targets) {
  strata_stats <- .estimate_tree_means_strata(handler, targets) |>
    dplyr::left_join(
      handler@pop_estn_unit |> dplyr::select(CN),
      by = c("ESTN_UNIT_CN" = "CN")
    )

  strat_keys <- c("STRATUM_CN", "ESTN_UNIT_CN", "EVAL_CN", "w_h", "n_h", "n")
  plot_keys <- c("PLT_CN", "STATECD", "INVYR", "PLOT", "COUNTYCD")

  all_cols <- colnames(strata_stats)
  group_vars <- setdiff(all_cols, c(plot_keys, strat_keys, targets))

  strata_stats |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("ESTN_UNIT_CN", group_vars)))) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(targets), ~ sum(w_h * .x, na.rm = TRUE))
    ) |>
    dplyr::ungroup()
}

.estimate_tree_means_strata <- function(handler, targets) {
  syms <- rlang::syms(targets)
  tree_values <- .make_tree_aggregates(handler, !!!syms, adjusted = TRUE)
  strata_summary <- .get_strata_summary(handler)

  combined_data <- tree_values %>%
    dplyr::inner_join(
      handler@pop_plot_stratum_assgn %>%
        dplyr::select(PLT_CN, STRATUM_CN),
      by = "PLT_CN"
    ) |>
    dplyr::inner_join(strata_summary, by = "STRATUM_CN")

  strat_keys <- c("STRATUM_CN", "ESTN_UNIT_CN", "EVAL_CN", "w_h", "n_h", "n")
  plot_keys <- c("PLT_CN", "STATECD", "INVYR", "PLOT", "COUNTYCD")

  all_cols <- colnames(combined_data)
  group_vars <- setdiff(all_cols, c(plot_keys, strat_keys, targets))

  combined_data %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(
          c("ESTN_UNIT_CN", "STRATUM_CN", "w_h", "n_h", "n", group_vars)
        )
      )
    ) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(targets), ~ sum(.x, na.rm = TRUE) / n_h)
    ) %>%
    dplyr::ungroup()
}
