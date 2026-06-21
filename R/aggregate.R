#' Complete Aggregation Scaffold
#'
#' Ensures that the aggregated result contains all expected combinations of plots and domain variables.
#'
#' @param plot_qry The base plot query (containing all plots to be retained).
#' @param aggregated_qry The query with aggregated results.
#' @param plot_keys A character vector of columns that uniquely identify a plot.
#' @param domain_vars A character vector of domain variables that form the scaffold with plot_keys.
#' @param sparse Logical. If TRUE, returns a sparse result (only observed combinations) to optimize performance. Defaults to FALSE.
#'
#' @return A lazy query with the full scaffold joined to the aggregated data.
#' @noRd
.complete_scaffold <- function(plot_qry, aggregated_qry, plot_keys, domain_vars, sparse = FALSE) {
  # 1. Identify Target Variables (Response variables)
  # Any column in the aggregated result that is NOT a plot key or a domain variable is a target variable.
  # We assume these are numeric and should be filled with 0 where missing.
  agg_cols <- colnames(aggregated_qry)
  target_vars <- setdiff(agg_cols, c(plot_keys, domain_vars))

  # 2. Get all plots (Full Plot List)
  all_plots <- plot_qry %>%
    dplyr::select(dplyr::all_of(plot_keys))

  if (sparse && length(domain_vars) > 0) {
    # Optimization: Densification with zeros is mathematically redundant for
    # summation aggregation and causes exponential data growth.
    # We return the aggregated data directly, ensuring it matches the plot list.
    return(
      aggregated_qry %>%
        dplyr::ungroup() %>%
        dplyr::semi_join(all_plots, by = plot_keys)
    )
  }

  if (length(domain_vars) > 0) {
    # Extract distinct combinations of domain variables from the aggregated data
    observed_domains <- aggregated_qry %>%
      dplyr::ungroup() %>%
      dplyr::select(dplyr::all_of(domain_vars)) %>%
      dplyr::distinct()

    # Cross join: All Plots x Observed Domains
    scaffold <- all_plots %>%
      dplyr::cross_join(observed_domains, copy = TRUE)

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

.resolve_partition_domains <- function(domains, scope, data_cols) {
  if (length(domains) == 0) {
    return(domains)
  }

  suffix <- switch(
    scope,
    plot = "",
    cond = ".cond",
    tree = ".tree",
    tree_history = ".tree_history",
    rlang::abort(sprintf("Unknown partition scope: '%s'.", scope))
  )

  resolved <- lapply(domains, function(domain) {
    expr <- rlang::get_expr(domain)

    if (!rlang::is_symbol(expr) || identical(suffix, "")) {
      return(domain)
    }

    scoped_name <- paste0(rlang::as_string(expr), suffix)
    if (!scoped_name %in% data_cols) {
      return(domain)
    }

    rlang::new_quosure(rlang::sym(scoped_name), env = rlang::get_env(domain))
  })

  names(resolved) <- names(domains)
  resolved
}

.group_by_missing_vars <- function(.data, vars) {
  missing_vars <- setdiff(vars, dplyr::group_vars(.data))

  if (length(missing_vars) == 0) {
    return(.data)
  }

  .data %>%
    dplyr::group_by(!!!rlang::syms(missing_vars), .add = TRUE)
}

.resolve_tree_target_names <- function(target_vars) {
  user_names <- names(target_vars)
  if (is.null(user_names) || length(user_names) == 0) {
    user_names <- rep("", length(target_vars))
  }

  vapply(seq_along(target_vars), function(i) {
    var_quo <- target_vars[[i]]

    if (length(user_names) >= i && nzchar(user_names[[i]])) {
      return(user_names[[i]])
    }

    expr <- rlang::quo_get_expr(var_quo)
    if (rlang::is_symbol(expr)) {
      return(rlang::as_string(expr))
    }

    if (rlang::is_call(expr)) {
      fn <- rlang::call_name(expr)
      arg <- expr[[2]]
      if (!is.null(fn) && !is.null(arg) && rlang::is_symbol(arg)) {
        return(paste0(fn, "_", rlang::as_string(arg)))
      }
    }

    rlang::as_label(var_quo)
  }, character(1))
}

.validate_expander_column <- function(.data, expander, target) {
  if (!is.character(expander) || length(expander) != 1 || is.na(expander) || expander == "") {
    stop("`expander` must resolve to exactly one column name.", call. = FALSE)
  }

  if (!expander %in% colnames(.data)) {
    stop(
      "`expander` column `", expander, "` was not found in ", target, " aggregation data.",
      call. = FALSE
    )
  }

  probe <- tryCatch(
    .data %>%
      dplyr::select(dplyr::all_of(expander)) %>%
      dplyr::collect(n = 1),
    error = function(e) {
      stop("Unable to validate `expander` column `", expander, "`: ", conditionMessage(e), call. = FALSE)
    }
  )

  if (ncol(probe) != 1 || !is.numeric(probe[[1]])) {
    stop("`expander` column `", expander, "` must be numeric.", call. = FALSE)
  }

  invisible(expander)
}


#' Aggregate condition data to plot or subplot levels
#'
#' @param object A EvalHandler object.
#' @keywords internal
.make_cond_aggregates <- function(object, adjusted = FALSE, sparse = FALSE) {
  res <- .build_cond_data(object)

  plot_keys <- .plot_keys_raw
  plot_domains <- .resolve_partition_domains(object@plot_domains, "plot", colnames(res))
  cond_domains <- .resolve_partition_domains(object@cond_domains, "cond", colnames(res))

  # Aggregate matching records
  # We group by PLOT keys + any user defined groups (in hierarchical order)
  if (length(plot_domains) > 0) {
    res <- res %>% dplyr::group_by(!!!plot_domains)
  }

  if (length(cond_domains) > 0) {
    res <- res %>% dplyr::group_by(!!!cond_domains, .add = TRUE)
  }

  # We also need to group by plot keys to ensure plot-level summary
  res <- .group_by_missing_vars(res, plot_keys)

  if (adjusted) {
    subptype_adj_factors <- .get_subptype_adjustment_factors(object)

    aggregated <- res |>
      dplyr::mutate(
        PROP_BASIS = dplyr::case_when(
          PROP_BASIS == "MACRO" ~ "MACR",
          TRUE ~ PROP_BASIS
        )
      ) |>
      dplyr::left_join(
        object@tables$pop_plot_stratum_assgn %>% dplyr::select(PLT_CN, STRATUM_CN),
        by = c("CN" = "PLT_CN")
      ) |>
      dplyr::left_join(
        subptype_adj_factors,
        by = c("STRATUM_CN", "PROP_BASIS" = "SUBPTYPE")
      ) |>
      dplyr::summarise(
        prop = dplyr::coalesce(sum(CONDPROP_UNADJ * ADJ_FACTOR, na.rm = TRUE), 0)
      )
  } else {
    aggregated <- res %>%
      dplyr::summarise(
        prop = sum(CONDPROP_UNADJ, na.rm = TRUE)
      ) %>%
      dplyr::ungroup()
  }

  # Identify Domain Variables
  all_groups <- dplyr::group_vars(res)
  domain_vars <- setdiff(all_groups, plot_keys)

  # Use helper to build complete scaffold and merge
  final_res <- .complete_scaffold(
    plot_qry = object@tables$plot,
    aggregated_qry = aggregated,
    plot_keys = plot_keys,
    domain_vars = domain_vars,
    sparse = sparse
  )

  final_res <- final_res %>%
    dplyr::rename(PLT_CN = CN)

  return(final_res)
}


.build_cond_data <- function(object) {
  # Start from prepared plot data
  res <- .build_plot_data(object)

  # Join to condition table
  res <- res %>%
    dplyr::inner_join(object@tables$cond, by = c("CN" = "PLT_CN"), suffix = c("", ".cond"))

  # Apply pending condition-level mutations
  if (length(object@cond_mutations) > 0) {
    res <- res %>% dplyr::mutate(!!!object@cond_mutations)
  }

  # Apply pending condition-level filters
  if (length(object@cond_filters) > 0) {
    res <- res %>% dplyr::filter(!!!object@cond_filters)
  }

  return(res)
}

#' Prepare plot-level data with mutations and filters applied
#' 
#' @keywords internal
.build_plot_data <- function(object) {
  res <- object@tables$plot

  # Apply plot-level mutations
  if (length(object@plot_mutations) > 0) {
    res <- res %>% dplyr::mutate(!!!object@plot_mutations)
  }

  # Apply plot-level filters
  if (length(object@plot_filters) > 0) {
    res <- res %>% dplyr::filter(!!!object@plot_filters)
  }

  return(res)
}

#' Aggregate tree data to plot or subplot levels. This provides an unadjusted
#' density for the plot.
#'
#' @param object A EvalHandler object.
#' @param ... Variables to aggregate (tidy-select supported)
#' @param expander Tree expansion column used for weighting tree-level sums.
#' @param level The level to aggregate to. Can be "plot" or "subplot".
#' @keywords internal
.make_tree_aggregates <- function(object, ..., expander = "TPA_UNADJ", adjusted = FALSE, level = "plot", sparse = FALSE) {
  res <- .build_tree_data(object)
  .validate_expander_column(res, expander, "tree")
  res <- res %>%
    dplyr::mutate(.expander_wt = .data[[expander]]) %>%
    dplyr::filter(!is.na(.expander_wt))

  plot_keys <- .plot_keys_raw
  plot_domains <- .resolve_partition_domains(object@plot_domains, "plot", colnames(res))
  cond_domains <- .resolve_partition_domains(object@cond_domains, "cond", colnames(res))
  tree_domains <- .resolve_partition_domains(object@tree_domains, "tree", colnames(res))

  # Determine target variables for aggregation
  target_vars <- dplyr::quos(...)

  # Check if "1" is in the targets (implicit stem density)
  vars_as_strings <- vapply(target_vars, rlang::as_label, character(1))
  if ("1" %in% vars_as_strings) {
    res <- res %>% dplyr::mutate(`1` = 1)
  }

  # Group by PLOT keys and user defined domains
  # Order: plot domains, cond domains, tree domains (hierarchical)
  if (length(plot_domains) > 0) {
    res <- res %>% dplyr::group_by(!!!plot_domains)
  }

  if (length(cond_domains) > 0) {
    res <- res %>% dplyr::group_by(!!!cond_domains, .add = TRUE)
  }

  if (length(tree_domains) > 0) {
    res <- res %>% dplyr::group_by(!!!tree_domains, .add = TRUE)
  }

  # We also need to group by plot keys to ensure plot-level summary
  res <- .group_by_missing_vars(res, plot_keys)

  # Perform the aggregation (summing)
  if (length(target_vars) == 0) {
    aggregated <- res %>%
      dplyr::summarise(
        tree_count = sum(.expander_wt, na.rm = TRUE)
      ) %>%
      dplyr::ungroup()
  } else {
    agg_exprs <- purrr::map(target_vars, function(var_quo) {
      expr <- rlang::quo_get_expr(var_quo)
      if (rlang::is_symbol(expr)) {
        rlang::expr(sum(TPA_UNADJ * (!!var_quo), na.rm = TRUE))
      } else {
        var_quo
      }
    })
    names(agg_exprs) <- .resolve_tree_target_names(target_vars)

    aggregated <- res %>%
      dplyr::summarise(
        dplyr::across(c(!!!target_vars), function(x) sum(.expander_wt * x, na.rm = TRUE))
      ) %>%
      dplyr::ungroup()
  }

  # Identify Domain Variables
  all_groups <- dplyr::group_vars(res)
  domain_vars <- setdiff(all_groups, plot_keys)

  # Use helper to build complete scaffold and merge
  final_res <- .complete_scaffold(
    plot_qry = object@tables$plot,
    aggregated_qry = aggregated,
    plot_keys = plot_keys,
    domain_vars = domain_vars,
    sparse = sparse
  )

  final_res <- final_res %>%
    dplyr::rename(PLT_CN = CN)

  return(final_res)
}

#' Aggregate tree history data to plot or subplot levels.
#'
#' Reserved for GRM-specific aggregation of tree history records.
#'
#' @param object A EvalHandler object.
#' @param ... Additional arguments.
#' @param expander Tree expansion column used for weighting tree-history sums.
#' @param sparse Logical. If TRUE, returns a sparse result.
#' @keywords internal
.make_tree_history_aggregates <- function(object, ..., expander = "TPA_UNADJ", sparse = FALSE) {
  res <- .build_tree_history_data(object)
  .validate_expander_column(res, expander, "tree_history")
  res <- res %>%
    dplyr::mutate(.expander_wt = .data[[expander]]) %>%
    dplyr::filter(!is.na(.expander_wt))

  plot_keys <- .plot_keys_raw
  plot_domains <- .resolve_partition_domains(object@plot_domains, "plot", colnames(res))
  cond_domains <- .resolve_partition_domains(object@cond_domains, "cond", colnames(res))
  tree_history_domains <- .resolve_partition_domains(object@tree_history_domains, "tree_history", colnames(res))

  # Determine target variables for aggregation
  target_vars <- dplyr::quos(...)

  # Check if "1" is in the targets (implicit stem density)
  vars_as_strings <- vapply(target_vars, rlang::as_label, character(1))
  if ("1" %in% vars_as_strings) {
    res <- res %>% dplyr::mutate(`1` = 1)
  }

  # Group by PLOT keys and user defined domains
  # Order: plot domains, cond domains, tree_history domains (hierarchical)
  if (length(plot_domains) > 0) {
    res <- res %>% dplyr::group_by(!!!plot_domains)
  }

  if (length(cond_domains) > 0) {
    res <- res %>% dplyr::group_by(!!!cond_domains, .add = TRUE)
  }

  if (length(tree_history_domains) > 0) {
    res <- res %>% dplyr::group_by(!!!tree_history_domains, .add = TRUE)
  }

  # We also need to group by plot keys to ensure plot-level summary
  res <- .group_by_missing_vars(res, plot_keys)

  # Perform the aggregation (summing)
  if (length(target_vars) == 0) {
    aggregated <- res %>%
      dplyr::summarise(
        tree_count = sum(.expander_wt, na.rm = TRUE)
      ) %>%
      dplyr::ungroup()
  } else {
    aggregated <- res %>%
      dplyr::summarise(
        dplyr::across(c(!!!target_vars), function(x) sum(.expander_wt * x, na.rm = TRUE))
      ) %>%
      dplyr::ungroup()
  }

  # Identify Domain Variables
  all_groups <- dplyr::group_vars(res)
  domain_vars <- setdiff(all_groups, plot_keys)

  # Use helper to build complete scaffold and merge
  final_res <- .complete_scaffold(
    plot_qry = object@tables$plot,
    aggregated_qry = aggregated,
    plot_keys = plot_keys,
    domain_vars = domain_vars,
    sparse = sparse
  )

  final_res <- final_res %>%
    dplyr::rename(PLT_CN = CN)

  return(final_res)
}

#' @keywords internal
.build_tree_data <- function(object) {
  # Start from prepared plot data (includes plot mutations/filters)
  res <- .build_plot_data(object)

  # Join to condition table, then tree table
  res <- res %>%
    dplyr::inner_join(object@tables$cond, by = c("CN" = "PLT_CN"), suffix = c("", ".cond")) %>%
    dplyr::inner_join(object@tables$tree, by = c("CN" = "PLT_CN", "CONDID" = "CONDID"), suffix = c("", ".tree"))

  # Join to reference species table if available
  if (!is.null(object@tables$ref_species)) {
    res <- res %>%
      dplyr::left_join(object@tables$ref_species, by = "SPCD", suffix = c("", ".ref"))
  } else {
    warning("REF_SPECIES table not available; continuing without species reference columns.", call. = FALSE)
  }

  # Apply condition-level mutations (these affect all trees in those conditions)
  if (length(object@cond_mutations) > 0) {
    res <- res %>% dplyr::mutate(!!!object@cond_mutations)
  }

  # Apply tree-level mutations (these affect individual tree records)
  if (length(object@tree_mutations) > 0) {
    res <- res %>% dplyr::mutate(!!!object@tree_mutations)
  }

  # Apply condition-level filters (these remove entire conditions and their trees)
  if (length(object@cond_filters) > 0) {
    res <- res %>% dplyr::filter(!!!object@cond_filters)
  }

  # Apply tree-level filters (these remove individual tree records)
  if (length(object@tree_filters) > 0) {
    res <- res %>% dplyr::filter(!!!object@tree_filters)
  }

  return(res)
}

#' @keywords internal
.build_tree_history_data <- function(object) {
  # Start from prepared plot data (includes plot mutations/filters)
  res <- .build_plot_data(object)

  # Join to condition table, then tree_history table
  res <- res %>%
    dplyr::inner_join(object@tables$cond, by = c("CN" = "PLT_CN"), suffix = c("", ".cond")) %>%
    dplyr::inner_join(object@tables$tree_history, by = c("CN" = "PLT_CN", "CONDID" = "CONDID"), suffix = c("", ".tree_history"))

  # Join to reference species table if available
  if (!is.null(object@tables$ref_species)) {
    res <- res %>%
      dplyr::left_join(object@tables$ref_species, by = "SPCD", suffix = c("", ".ref"))
  } else {
    warning("REF_SPECIES table not available; continuing without species reference columns.", call. = FALSE)
  }

  # Apply condition-level mutations (these affect all trees in those conditions)
  if (length(object@cond_mutations) > 0) {
    res <- res %>% dplyr::mutate(!!!object@cond_mutations)
  }

  # Apply tree-history-level mutations (these affect individual tree history records)
  if (length(object@tree_history_mutations) > 0) {
    res <- res %>% dplyr::mutate(!!!object@tree_history_mutations)
  }

  # Apply condition-level filters (these remove entire conditions and their trees)
  if (length(object@cond_filters) > 0) {
    res <- res %>% dplyr::filter(!!!object@cond_filters)
  }

  # Apply tree-history-level filters (these remove individual tree history records)
  if (length(object@tree_history_filters) > 0) {
    res <- res %>% dplyr::filter(!!!object@tree_history_filters)
  }

  return(res)
}
