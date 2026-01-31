#' Aggregate condition data to plot or subplot levels
#'
#' @param object A EvalHandler object.
#' @keywords internal
.make_cond_aggregates <- function(object, adjusted = FALSE, sparse = FALSE) {
  res <- .build_cond_data(object)

  # Standard PLOT keys
  plot_keys <- c("CN", "STATECD", "INVYR", "PLOT", "COUNTYCD")

  # Aggregate matching records
  # We group by PLOT keys + any user defined groups
  if (length(object@cond_domains) > 0) {
    res <- res %>% dplyr::group_by(!!!object@cond_domains)
  }

  # We also need to group by plot keys to ensure plot-level summary
  res <- res %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(plot_keys)), .add = TRUE)

  if (adjusted) {
    subptype_adj_factors <- .get_subptype_adjustment_factors(object)

    aggregated <- res |>
      dplyr::left_join(
        object@tables$pop_plot_stratum_assgn %>% dplyr::select(PLT_CN, STRATUM_CN),
        by = c("CN" = "PLT_CN")
      ) |>
      dplyr::left_join(
        subptype_adj_factors,
        by = c("STRATUM_CN", "PROP_BASIS" = "SUBPTYPE")
      ) |>
      dplyr::summarise(
        prop = sum(CONDPROP_UNADJ * ADJ_FACTOR, na.rm = TRUE)
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
  res <- object@tables$plot %>%
    dplyr::inner_join(object@tables$cond, by = c("CN" = "PLT_CN"), suffix = c("", ".cond"))

  # Apply pending mutations from the handler object
  if (length(object@cond_mutations) > 0) {
    res <- res %>% dplyr::mutate(!!!object@cond_mutations)
  }

  if (length(object@cond_filters) > 0) {
    res <- res %>% dplyr::filter(!!!object@cond_filters)
  }

  return(res)
}

#' Aggregate tree data to plot or subplot levels. This provides an unadjusted
#' density for the plot.
#'
#' @param object A EvalHandler object.
#' @param ... Variables to aggregate (tidy-select supported)
#' @param level The level to aggregate to. Can be "plot" or "subplot".
#' @keywords internal
.make_tree_aggregates <- function(object, ..., adjusted = FALSE, level = "plot", sparse = FALSE) {
  res <- .build_tree_data(object)

  plot_keys <- c("CN", "STATECD", "INVYR", "PLOT", "COUNTYCD")

  # Determine target variables for aggregation
  target_vars <- dplyr::quos(...)

  # Group by PLOT keys and user defined domains
  if (length(object@cond_domains) > 0) {
    res <- res %>% dplyr::group_by(!!!object@cond_domains)
  }

  if (length(object@tree_domains) > 0) {
    res <- res %>% dplyr::group_by(!!!object@tree_domains, .add = TRUE)
  }

  # We also need to group by plot keys to ensure plot-level summary
  res <- res %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(plot_keys)), .add = TRUE)

  # Perform the aggregation (summing)
  if (length(target_vars) == 0) {
    aggregated <- res %>%
      dplyr::summarise(
        tree_count = sum(TPA_UNADJ, na.rm = TRUE)
      ) %>%
      dplyr::ungroup()
  } else {
    aggregated <- res %>%
      dplyr::summarise(
        dplyr::across(c(!!!target_vars), function(x) sum(TPA_UNADJ * x, na.rm = TRUE))
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
  res <- object@tables$plot %>%
    dplyr::inner_join(object@tables$cond, by = c("CN" = "PLT_CN"), suffix = c("", ".cond")) %>%
    dplyr::inner_join(object@tables$tree, by = c("CN" = "PLT_CN", "CONDID" = "CONDID"), suffix = c("", ".tree"))

  # Apply standard filtering
  res <- res %>%
    dplyr::filter(
      !is.na(TPA_UNADJ)
    )

  # Apply pending mutations from the handler object
  if (length(object@cond_mutations) > 0) {
    res <- res %>% dplyr::mutate(!!!object@cond_mutations)
  }

  # These are user-defined expressions queued via mutate_tree()
  if (length(object@tree_mutations) > 0) {
    res <- res %>% dplyr::mutate(!!!object@tree_mutations)
  }

  if (length(object@tree_filters) > 0) {
    res <- res %>% dplyr::filter(!!!object@tree_filters)
  }

  return(res)
}

#' Aggregate change tree data to plot level
#'
#' @param object A EvalHandler object.
#' @param ... Variables to aggregate (tidy-select supported)
#' @param sparse Logical. If TRUE, returns a sparse result.
#' @keywords internal
.make_change_tree_aggregates <- function(object, ..., sparse = FALSE) {
  # 1. Parse Targets
  quos <- dplyr::quos(...)
  # targets are likely symbols corresponding to strings like "b(VOL)"
  # We convert them to strings
  target_strs <- vapply(quos, function(q) rlang::as_label(rlang::quo_get_expr(q)), character(1))

  # Parse structure: "b(VOL)" -> type="b", var="VOL"
  parsed_targets <- lapply(target_strs, function(s) {
    if (grepl("^b\\((.*)\\)$", s)) {
      list(type = "b", var = gsub("^b\\((.*)\\)$", "\\1", s), orig = s)
    } else if (grepl("^m\\((.*)\\)$", s)) {
      list(type = "m", var = gsub("^m\\((.*)\\)$", "\\1", s), orig = s)
    } else if (grepl("^e\\((.*)\\)$", s)) {
      list(type = "e", var = gsub("^e\\((.*)\\)$", "\\1", s), orig = s)
    } else {
      # Default to component/end if no wrapper
      list(type = "e", var = s, orig = s)
    }
  })

  # 2. Build Data Query
  # Start with PLOT -> COND -> TREE_GRM_COMPONENT
  res <- object@tables$plot %>%
    dplyr::inner_join(object@tables$cond, by = c("CN" = "PLT_CN"), suffix = c("", ".cond"))

  comp <- object@tables$tree_grm_component

  types_needed <- unique(vapply(parsed_targets, function(x) x$type, character(1)))

  # Prepare 'comp' query: Select keys + TPA + 'e' vars
  e_vars <- unique(vapply(parsed_targets[vapply(parsed_targets, function(x) x$type == "e", logical(1))], function(x) x$var, character(1)))
  comp_cols <- unique(c("PLT_CN", "TRE_CN", "CONDID", "TPA_UNADJ", e_vars))
  comp_q <- comp %>% dplyr::select(dplyr::all_of(comp_cols))

  # Join to base
  res <- res %>%
    dplyr::inner_join(comp_q, by = c("CN" = "PLT_CN", "CONDID" = "CONDID"))

  # Join Begin if needed
  if ("b" %in% types_needed) {
    b_vars <- unique(vapply(parsed_targets[vapply(parsed_targets, function(x) x$type == "b", logical(1))], function(x) x$var, character(1)))
    b_cols <- c("PLT_CN", "TRE_CN", b_vars)

    b_q <- object@tables$tree_grm_begin %>%
      dplyr::select(dplyr::all_of(b_cols)) %>%
      dplyr::rename_with(~ paste0("b_", .), dplyr::all_of(b_vars))

    res <- res %>%
      dplyr::left_join(b_q, by = c("CN" = "PLT_CN", "TRE_CN" = "TRE_CN"))
  }

  # Join Mid if needed
  if ("m" %in% types_needed) {
    m_vars <- unique(vapply(parsed_targets[vapply(parsed_targets, function(x) x$type == "m", logical(1))], function(x) x$var, character(1)))
    m_cols <- c("PLT_CN", "TRE_CN", m_vars)

    m_q <- object@tables$tree_grm_midpt %>%
      dplyr::select(dplyr::all_of(m_cols)) %>%
      dplyr::rename_with(~ paste0("m_", .), dplyr::all_of(m_vars))

    res <- res %>%
      dplyr::left_join(m_q, by = c("CN" = "PLT_CN", "TRE_CN" = "TRE_CN"))
  }

  # Apply mutations/filters
  if (length(object@cond_mutations) > 0) {
    res <- res %>% dplyr::mutate(!!!object@cond_mutations)
  }

  if (length(object@tree_mutations) > 0) {
    res <- res %>% dplyr::mutate(!!!object@tree_mutations)
  }

  if (length(object@tree_filters) > 0) {
    res <- res %>% dplyr::filter(!!!object@tree_filters)
  }

  # 3. Aggregate
  plot_keys <- c("CN", "STATECD", "INVYR", "PLOT", "COUNTYCD")

  if (length(object@cond_domains) > 0) {
    res <- res %>% dplyr::group_by(!!!object@cond_domains)
  }

  if (length(object@tree_domains) > 0) {
    res <- res %>% dplyr::group_by(!!!object@tree_domains, .add = TRUE)
  }

  res <- res %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(plot_keys)), .add = TRUE)

  # Construct summary expressions
  summary_exprs <- lapply(parsed_targets, function(pt) {
    col_name <- if (pt$type == "b") paste0("b_", pt$var)
           else if (pt$type == "m") paste0("m_", pt$var)
           else pt$var

    rlang::expr(sum(TPA_UNADJ * !!rlang::sym(col_name), na.rm = TRUE))
  })

  names(summary_exprs) <- target_strs

  aggregated <- res %>%
    dplyr::summarise(!!!summary_exprs) %>%
    dplyr::ungroup()

  # 4. Scaffold
  all_groups <- dplyr::group_vars(res)
  domain_vars <- setdiff(all_groups, plot_keys)

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
