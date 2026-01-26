setClass("PostStratifiedEstimator",
  contains = "Estimator",
  slots = list(
    strata_weights = "ANY"
  )
)

#' Constructor for PostStratifiedEstimator
#'
#' @param handler A EvalHandler object.
#' @export
PostStratifiedEstimator <- function(handler) {
  # Calculate strata weights
  strata_weights <- handler@pop_stratum %>%
    dplyr::inner_join(
      handler@pop_estn_unit,
      by = c("ESTN_UNIT_CN" = "CN"),
      suffix = c("", ".eu")
    ) %>%
    dplyr::mutate(
      w_h = as.numeric(P1POINTCNT) / P1PNTCNT_EU
    ) %>%
    dplyr::select(
      STRATUM_CN = CN, ESTN_UNIT_CN, w_h, P2POINTCNT, AREA_USED
    )

  new("PostStratifiedEstimator",
    handler = handler,
    strata_weights = strata_weights
  )
}


#' Estimate Population Parameters
#'
#' @param object A PostStratifiedEstimator object.
#' @param ... A formula specifying the estimation target (e.g., tree ~ VOLCFGRS).
#' @return A dataframe with estimates.
#' @export
setMethod("estimate", "PostStratifiedEstimator", function(object, ...) {
  args <- list(...)
  if (length(args) == 0) stop("Must provide a formula.")
  formula <- args[[1]]

  parsed <- parse_formula(formula)
  slot_name <- parsed$slot
  targets <- parsed$targets

  if (slot_name == "cond") {
    if (!all(targets == "1")) {
      stop("Only 'cond ~ 1' is currently supported for condition estimates.")
    }
    return(.estimate_cond_internal(object))
  } else if (slot_name == "tree") {
    return(.estimate_tree_internal(object, targets))
  } else {
    stop("Unsupported slot: ", slot_name)
  }
})

# Internal helper for condition estimation
.estimate_cond_internal <- function(object) {
  # Calculate eu weights
  eu_weights <- object@handler@pop_estn_unit |>
    dplyr::mutate(
      w_eu = P1PNTCNT_EU / sum(P1PNTCNT_EU, na.rm = TRUE)
    ) |>
    dplyr::select(CN, w_eu)

  eu_stats <- .estimate_cond_eu_internal(object)

  eu_keys <- c("ESTN_UNIT_CN")

  # All columns in eu_stats
  all_cols <- colnames(eu_stats)
  group_vars <- setdiff(all_cols, c(eu_keys, "p_eu", "w_eu"))

  eu_stats |>
    dplyr::left_join(eu_weights, by = c("ESTN_UNIT_CN" = "CN")) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      p = sum(p_eu * w_eu, na.rm = TRUE)
    ) |>
    dplyr::ungroup()
}

.estimate_cond_eu_internal <- function(object) {
  strata_stats <- .estimate_cond_strata_internal(object) |>
    dplyr::left_join(
      object@handler@pop_estn_unit |> dplyr::select(CN),
      by = c("ESTN_UNIT_CN" = "CN")
    )

  strat_keys <- c("STRATUM_CN", "ESTN_UNIT_CN", "EVAL_CN", "w_h", "n_h", "n")
  plot_keys <- c("PLT_CN", "STATECD", "INVYR", "PLOT", "COUNTYCD")

  # All columns in strata_stats
  all_cols <- colnames(strata_stats)
  group_vars <- setdiff(all_cols, c(plot_keys, strat_keys, "p_strat"))

  strata_stats |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("ESTN_UNIT_CN", group_vars)))) |>
    dplyr::summarise(
      p_eu = sum(w_h * p_strat, na.rm = TRUE)
    ) |>
    dplyr::ungroup()
}

.estimate_cond_strata_internal <- function(object) {
  cond_values <- .make_cond_aggregates(object@handler, adjusted = TRUE)
  strata_summary <- .get_strata_summary(object@handler)

  combined_data <- cond_values %>%
    dplyr::inner_join(
      object@handler@pop_plot_stratum_assgn %>%
        dplyr::select(PLT_CN, STRATUM_CN),
      by = "PLT_CN"
    ) |>
    dplyr::inner_join(strata_summary, by = "STRATUM_CN")

  strat_keys <- c("STRATUM_CN", "ESTN_UNIT_CN", "EVAL_CN", "w_h", "n_h", "n")
  plot_keys <- c("PLT_CN", "STATECD", "INVYR", "PLOT", "COUNTYCD")

  all_cols <- colnames(combined_data)
  group_vars <- setdiff(all_cols, c(plot_keys, strat_keys, "prop"))

  combined_data %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(
          c("ESTN_UNIT_CN", "STRATUM_CN", "w_h", "n_h", "n", group_vars)
        )
      )
    ) %>%
    dplyr::summarise(
      p_strat = sum(prop, na.rm = TRUE) / n_h
    ) %>%
    dplyr::ungroup()
}

# Internal helper for tree estimation
.estimate_tree_internal <- function(object, targets) {
  # Calculate eu weights
  eu_weights <- object@handler@pop_estn_unit |>
    dplyr::mutate(
      w_eu = P1PNTCNT_EU / sum(P1PNTCNT_EU, na.rm = TRUE)
    ) |>
    dplyr::select(CN, w_eu)

  eu_stats <- .estimate_tree_eu_internal(object, targets)

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

.estimate_tree_eu_internal <- function(object, targets) {
  strata_stats <- .estimate_tree_strata_internal(object, targets) |>
    dplyr::left_join(
      object@handler@pop_estn_unit |> dplyr::select(CN),
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

.estimate_tree_strata_internal <- function(object, targets) {
  syms <- rlang::syms(targets)
  tree_values <- .make_tree_aggregates(object@handler, !!!syms, adjusted = TRUE)
  strata_summary <- .get_strata_summary(object@handler)

  combined_data <- tree_values %>%
    dplyr::inner_join(
      object@handler@pop_plot_stratum_assgn %>%
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
