#' Complete Aggregation Scaffold
#'
#' Ensures that the aggregated result contains all expected combinations of plots and domain variables.
#'
#' @param plot_qry The base plot query (containing all plots to be retained).
#' @param aggregated_qry The query with aggregated results.
#' @param plot_keys A character vector of columns that uniquely identify a plot (e.g., c("CN", "STATECD", "INVYR", "PLOT", "COUNTYCD")).
#' @param domain_vars A character vector of grouping variables (domains) that form the scaffold with plot_keys.
#'
#' @return A lazy query with the full scaffold joined to the aggregated data.
#' @noRd
.complete_scaffold <- function(plot_qry, aggregated_qry, plot_keys, domain_vars) {
  # 1. Get all plots (Full Plot List)
  all_plots <- plot_qry %>%
    dplyr::select(dplyr::all_of(plot_keys))

  # 2. Identify Target Variables (Response variables)
  # Any column in the aggregated result that is NOT a plot key or a domain variable is a target variable.
  # We assume these are numeric and should be filled with 0 where missing.
  agg_cols <- colnames(aggregated_qry)
  target_vars <- setdiff(agg_cols, c(plot_keys, domain_vars))

  # Construct the scaffold
  if (length(domain_vars) > 0) {
    # Extract distinct combinations of domain variables from the aggregated data
    observed_domains <- aggregated_qry %>%
      dplyr::ungroup() %>%
      dplyr::select(dplyr::all_of(domain_vars)) %>%
      dplyr::distinct()

    # Cross join: All Plots x Observed Domains
    scaffold <- all_plots %>%
      dplyr::cross_join(observed_domains)

    # Join key
    join_by <- c(plot_keys, domain_vars)
  } else {
    scaffold <- all_plots
    join_by <- plot_keys
  }

  # Left join aggregated data onto the scaffold
  final_res <- scaffold %>%
    dplyr::left_join(aggregated_qry, by = join_by)

  # Fill NAs with 0 for target variables
  if (length(target_vars) > 0) {
    final_res <- final_res %>%
      dplyr::mutate(
        dplyr::across(dplyr::all_of(target_vars), ~ dplyr::coalesce(.x, 0))
      )
  }

  return(final_res)
}
