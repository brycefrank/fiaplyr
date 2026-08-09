#' Complete Aggregation Scaffold
#'
#' Ensures that the aggregated result contains all expected combinations of plots and domain variables.
#'
#' @param plot_qry The base plot query (containing all plots to be retained).
#' @param aggregated_qry The query with aggregated results.
#' @param plot_keys A character vector of columns that uniquely identify a plot.
#' @param domain_vars A character vector of domain variables that form the scaffold with plot_keys.
#' @param sparse Logical. If TRUE, returns a sparse result (only observed combinations) to optimize performance. Defaults to FALSE.
#' @param zero_fill_vars Character vector of target column names that should be filled with 0 for missing plots.
#'   Passthrough (user-supplied summarise) columns are left as NA. Defaults to all target variables.
#'
#' @return A lazy query with the full scaffold joined to the aggregated data.
#' @noRd
.complete_scaffold <- function(
  plot_qry,
  aggregated_qry,
  plot_keys,
  domain_vars,
  sparse = FALSE,
  zero_fill_vars = NULL
) {
  # Any column in the aggregated result that is NOT a plot key or a domain variable is a target variable.
  agg_cols <- colnames(aggregated_qry)
  target_vars <- setdiff(agg_cols, c(plot_keys, domain_vars))

  # Default: zero-fill all target vars (preserves existing behaviour for callers that don't pass zero_fill_vars)
  if (is.null(zero_fill_vars)) {
    zero_fill_vars <- target_vars
  }

  # 2. Get all plots (Full Plot List)
  all_plots <- plot_qry %>%
    dplyr::select(dplyr::all_of(plot_keys))

  if (sparse && length(domain_vars) > 0) {
    return(
      aggregated_qry %>%
        dplyr::ungroup() %>%
        dplyr::semi_join(all_plots, by = plot_keys)
    )
  }

  if (length(domain_vars) > 0) {
    observed_domains <- aggregated_qry %>%
      dplyr::ungroup() %>%
      dplyr::select(dplyr::all_of(domain_vars)) %>%
      dplyr::distinct()

    scaffold <- all_plots %>%
      dplyr::cross_join(observed_domains, copy = TRUE)

    join_by <- c(plot_keys, domain_vars)
  } else {
    scaffold <- all_plots
    join_by <- plot_keys
  }

  final_res <- scaffold %>%
    dplyr::left_join(aggregated_qry, by = join_by)

  # Only zero-fill columns that came from TPA expansion or macros
  cols_to_zero <- intersect(zero_fill_vars, target_vars)
  if (length(cols_to_zero) > 0) {
    final_res <- final_res %>%
      dplyr::mutate(
        dplyr::across(dplyr::all_of(cols_to_zero), ~ dplyr::coalesce(.x, 0))
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
    dwm = ".dwm",
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

  vapply(
    seq_along(target_vars),
    function(i) {
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
    },
    character(1)
  )
}

.eval_as_target <- function(var_quo) {
  # Try the quosure's captured environment first
  result <- tryCatch(rlang::eval_tidy(var_quo), error = function(e) NULL)
  if (inherits(result, "fiaplyr_target")) {
    return(result)
  }
  # Fall back to evaluating the expression in the fiaplyr namespace.
  # This handles cases where the quosure's environment (e.g., inside an S4
  # method body) doesn't have the package's exported symbols on its search path.
  expr <- rlang::quo_get_expr(var_quo)
  tryCatch(eval(expr, envir = asNamespace("fiaplyr")), error = function(e) NULL)
}

.macro_adjustment_factor_expr <- function(macro, adjusted) {
  macro_adjust <- if (is.null(macro$adjust)) "auto" else macro$adjust
  macro_basis <- if (is.null(macro$adjust_basis)) {
    "subptyp_grm"
  } else {
    macro$adjust_basis
  }
  unknown_subptype <- if (is.null(macro$unknown_subptype)) {
    "zero"
  } else {
    macro$unknown_subptype
  }

  if (macro_adjust == "none") {
    return(rlang::expr(1))
  }

  if (!adjusted) {
    return(rlang::expr(1))
  }

  if (!macro_adjust %in% c("auto", "subptype")) {
    stop("Unsupported macro adjustment mode: ", macro_adjust, call. = FALSE)
  }

  if (!identical(macro_basis, "subptyp_grm")) {
    stop("Unsupported macro adjustment basis: ", macro_basis, call. = FALSE)
  }

  if (identical(unknown_subptype, "drop")) {
    return(rlang::expr(ADJ_FACTOR))
  }

  if (identical(unknown_subptype, "warn")) {
    warning(
      "Unknown GRM subtypes are being treated as zero-adjustment rows for this macro target.",
      call. = FALSE
    )
  }

  rlang::expr(dplyr::coalesce(ADJ_FACTOR, 0))
}

.uses_default_expander_filter <- function(target_vars) {
  if (length(target_vars) == 0) {
    return(TRUE)
  }

  # Only apply the TPA_UNADJ IS NOT NULL filter when ALL targets use TPA expansion
  # (bare symbols or the literal 1). Macros and passthrough expressions manage
  # their own expansion logic and must not have rows silently dropped.
  all(vapply(
    target_vars,
    function(var_quo) {
      expr <- rlang::quo_get_expr(var_quo)
      rlang::is_symbol(expr) ||
        (is.numeric(expr) && length(expr) == 1 && !is.na(expr) && expr == 1)
    },
    logical(1)
  ))
}

#' Apply queued augmentations (external-data joins) to a lazy query
#'
#' @param res A lazy query (or data frame) to join onto.
#' @param augmentations A list of augmentation specs produced by
#'   `.parse_augment_helper()`.
#' @return The query with all augmentation joins applied.
#' @keywords internal
.apply_augmentations <- function(res, augmentations) {
  if (length(augmentations) == 0) {
    return(res)
  }

  for (aug in augmentations) {
    join_fn <- switch(
      aug$type,
      left = dplyr::left_join,
      inner = dplyr::inner_join,
      right = dplyr::right_join,
      full = dplyr::full_join,
      rlang::abort(sprintf(
        "Invalid join type '%s'. Must be one of 'left', 'inner', 'right', or 'full'.",
        aug$type
      ))
    )

    copy <- aug$copy
    if (is.null(copy)) {
      res_is_lazy <- inherits(res, "tbl_lazy")
      data_is_local <- !inherits(aug$data, "tbl_lazy")
      copy <- res_is_lazy && data_is_local

      if (copy) {
        n_rows <- tryCatch(nrow(aug$data), error = function(e) NA_integer_)
        warning(
          sprintf(
            paste0(
              "augment() is copying a local table (%s rows) into the remote database. ",
              "For large tables, pre-load the data into the database and pass a lazy ",
              "table reference, or set `copy = FALSE` explicitly."
            ),
            if (is.na(n_rows)) "unknown" else format(n_rows, big.mark = ",")
          ),
          call. = FALSE
        )
      }
    }

    res <- join_fn(
      res,
      aug$data,
      by = aug$by,
      copy = copy,
      suffix = c("", ".aug")
    )
  }

  res
}

.apply_level_pipeline <- function(res, object, target) {
  ops <- object@pipeline[[target]]

  res <- .apply_augmentations(res, ops$augment)

  if (length(ops$mutate) > 0) {
    res <- res %>% dplyr::mutate(!!!ops$mutate)
  }

  if (length(ops$filter) > 0) {
    res <- res %>% dplyr::filter(!!!ops$filter)
  }

  res
}

# Parse and validate the arguments to `aggregate()`, producing one parsed
# target spec per scope. Scope availability is validated against the
# analysis spec (e.g. `tree_history()` requires GRM, `dwm()` requires DWM).
.aggregate_prepare <- function(args, spec) {
  arg_names <- names(args)
  unnamed <- if (is.null(arg_names)) {
    rep(TRUE, length(args))
  } else {
    arg_names == ""
  }

  named_args <- if (is.null(arg_names)) {
    character(0)
  } else {
    arg_names[!unnamed & nzchar(arg_names)]
  }
  unknown_named <- setdiff(named_args, "sparse")
  if (length(unknown_named) > 0) {
    stop(
      "Unknown named argument(s) for `aggregate()`: ",
      paste(unknown_named, collapse = ", "),
      call. = FALSE
    )
  }
  sparse <- if ("sparse" %in% names(args)) args[["sparse"]] else FALSE

  if (!any(unnamed)) {
    stop(
      "Must provide at least one scope such as `tree(VOLCFGRS)`, `cond()`, or `tree_history(...)`."
    )
  }

  parsed_list <- lapply(args[unnamed], .parse_target_spec, caller = "aggregate")
  .validate_aggregate_scopes(spec, parsed_list)

  list(parsed_list = parsed_list, sparse = sparse)
}

.supported_aggregate_scopes <- function(spec) {
  if (inherits(spec, "DWMAnalysis")) {
    return(c("cond", "dwm"))
  }
  if (inherits(spec, "GRMAnalysis")) {
    return(c("tree", "cond", "tree_history"))
  }
  c("tree", "cond")
}

.validate_aggregate_scopes <- function(spec, parsed_list) {
  slots <- vapply(parsed_list, `[[`, character(1), "slot")
  supported <- .supported_aggregate_scopes(spec)
  for (slot in slots) {
    if (!slot %in% supported) {
      .stop_unsupported_aggregate_scope(spec, slot)
    }
  }
  invisible(parsed_list)
}

.stop_unsupported_aggregate_scope <- function(spec, slot) {
  if (inherits(spec, "DWMAnalysis")) {
    stop(
      "`dwm_analysis()` supports DWM component helpers and `cond()` aggregation, not `",
      slot,
      "()`.",
      call. = FALSE
    )
  }
  stop("Unsupported slot: ", slot, call. = FALSE)
}

# Build a combined plot-level aggregate for a set of scopes.
# Targets on the same scope are merged into a single aggregate; different
# scopes are combined with a full join on plot keys and shared domain columns.
.aggregate_combined <- function(handler, parsed_list, sparse) {
  slots <- vapply(parsed_list, `[[`, character(1), "slot")
  groups <- split(seq_along(parsed_list), slots)

  scope_info <- lapply(groups, function(idx) {
    group <- parsed_list[idx]
    list(
      table = .aggregate_scope(handler, group, sparse),
      target_cols = .scope_target_cols(group)
    )
  })

  .combine_scope_tables(scope_info)
}

.aggregate_scope <- function(handler, group, sparse) {
  slot <- group[[1]]$slot

  if (slot == "cond") {
    if (length(group) > 1) {
      stop(
        "`aggregate()` does not support multiple `cond()` helpers in one call.",
        call. = FALSE
      )
    }
    parsed <- group[[1]]
    if (length(parsed$targets) > 0 && !all(parsed$targets == "1")) {
      stop(
        "Only `aggregate(cond())` or `aggregate(cond(1))` is currently supported for condition aggregation."
      )
    }
    res <- .make_cond_aggregates(handler, sparse = sparse)
    if (length(parsed$target_names) == 1 && nzchar(parsed$target_names[[1]])) {
      res <- res %>% dplyr::rename(!!parsed$target_names[[1]] := prop)
    }
    return(res)
  }

  if (slot == "tree") {
    quosures <- .merge_tree_quosures(group)
    if (length(quosures) == 0) {
      return(.make_tree_aggregates(handler, sparse = sparse))
    }
    return(.make_tree_aggregates(handler, !!!quosures, sparse = sparse))
  }

  if (slot == "tree_history") {
    quosures <- .merge_tree_quosures(group)
    if (length(quosures) == 0) {
      return(.make_tree_history_aggregates(handler, sparse = sparse))
    }
    return(.make_tree_history_aggregates(handler, !!!quosures, sparse = sparse))
  }

  if (slot == "dwm") {
    dwm_targets <- unlist(lapply(group, `[[`, "dwm_targets"), recursive = FALSE)
    return(.make_dwm_aggregates(handler, dwm_targets, sparse = sparse))
  }

  stop("Unsupported slot: ", slot)
}

# Merge the quosures of multiple same-scope target helpers into one list.
# A literal `1` target (implicit stem density) is named `tree_count` so it
# survives merging and matches the empty-target column name.
.merge_tree_quosures <- function(group) {
  parts <- lapply(group, function(parsed) {
    qs <- parsed$quosures
    if (is.null(qs)) {
      qs <- rlang::syms(parsed$targets)
    }
    qs
  })
  quosures <- unlist(parts, recursive = FALSE)
  if (length(quosures) == 0) {
    return(quosures)
  }

  nm <- names(quosures)
  if (is.null(nm)) {
    nm <- rep("", length(quosures))
  }
  for (i in seq_along(quosures)) {
    expr <- rlang::quo_get_expr(quosures[[i]])
    if (is.numeric(expr) && length(expr) == 1 && !is.na(expr) && expr == 1) {
      nm[i] <- "tree_count"
    }
  }
  names(quosures) <- nm
  quosures
}

# Compute the output value-column names produced by an aggregate scope group.
# Used to identify domain columns and to guard against cross-scope collisions.
.scope_target_cols <- function(group) {
  slot <- group[[1]]$slot

  if (slot == "cond") {
    parsed <- group[[1]]
    if (length(parsed$target_names) == 1 && nzchar(parsed$target_names[[1]])) {
      return(parsed$target_names[[1]])
    }
    return("prop")
  }

  if (slot == "dwm") {
    targets <- unlist(lapply(group, `[[`, "dwm_targets"), recursive = FALSE)
    return(unname(vapply(targets, function(t) t$name, character(1))))
  }

  if (slot %in% c("tree", "tree_history")) {
    quosures <- .merge_tree_quosures(group)
    if (length(quosures) == 0) {
      return("tree_count")
    }
    return(.resolve_tree_target_names(quosures))
  }

  character(0)
}

.combine_scope_tables <- function(scope_info) {
  if (length(scope_info) == 1) {
    return(scope_info[[1]]$table)
  }

  all_targets <- unlist(lapply(scope_info, `[[`, "target_cols"))
  dupes <- unique(all_targets[duplicated(all_targets)])
  if (length(dupes) > 0) {
    stop(
      "Conflicting target column name(s) across scopes: ",
      paste(dupes, collapse = ", "),
      ". Rename one of the targets.",
      call. = FALSE
    )
  }

  Reduce(.join_scope_tables, scope_info)$table
}

.join_scope_tables <- function(acc_info, next_info) {
  acc <- acc_info$table
  next_tbl <- next_info$table

  domains_acc <- setdiff(colnames(acc), c(.plot_keys, acc_info$target_cols))
  domains_next <- setdiff(colnames(next_tbl), c(.plot_keys, next_info$target_cols))
  shared_domains <- intersect(domains_acc, domains_next)

  joined <- dplyr::full_join(
    acc,
    next_tbl,
    by = c(.plot_keys, shared_domains)
  )

  list(
    table = joined,
    target_cols = c(acc_info$target_cols, next_info$target_cols)
  )
}

#' Aggregate condition data to plot or subplot levels
#'
#' @param object A EvalHandler object.
#' @keywords internal
.make_cond_aggregates <- function(object, adjusted = FALSE, sparse = FALSE) {
  res <- .build_cond_data(object)

  plot_keys <- .plot_keys_raw
  plot_domains <- .resolve_partition_domains(
    .pipeline_domains(object, "plot"),
    "plot",
    colnames(res)
  )
  cond_domains <- .resolve_partition_domains(
    .pipeline_domains(object, "cond"),
    "cond",
    colnames(res)
  )

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
        object@tables$pop_plot_stratum_assgn %>%
          dplyr::select(PLT_CN, STRATUM_CN),
        by = c("CN" = "PLT_CN")
      ) |>
      dplyr::left_join(
        subptype_adj_factors,
        by = c("STRATUM_CN", "PROP_BASIS" = "SUBPTYPE")
      ) |>
      dplyr::summarise(
        prop = dplyr::coalesce(
          sum(CONDPROP_UNADJ * ADJ_FACTOR, na.rm = TRUE),
          0
        )
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
    dplyr::inner_join(
      object@tables$cond,
      by = c("CN" = "PLT_CN"),
      suffix = c("", ".cond")
    )

  res <- .apply_level_pipeline(res, object, "cond")

  return(res)
}

#' Prepare joined condition-level DWM data
#'
#' @param object A DWM EvalHandler object.
#' @return A lazy query containing plot, condition, and DWM columns.
#' @keywords internal
.build_dwm_data <- function(object) {
  if (is.null(object@tables$cond_dwm_calc)) {
    stop("`COND_DWM_CALC` is not available for this analysis spec.", call. = FALSE)
  }

  res <- .build_plot_data(object) %>%
    dplyr::inner_join(
      object@tables$cond,
      by = c("CN" = "PLT_CN"),
      suffix = c("", ".cond")
    ) %>%
    dplyr::inner_join(
      object@tables$cond_dwm_calc,
      by = c("CN" = "PLT_CN", "CONDID"),
      suffix = c("", ".dwm")
    )

  res <- .apply_level_pipeline(res, object, "cond")
  .apply_level_pipeline(res, object, "dwm")
}

#' Aggregate downed woody material to plots
#'
#' @param object A DWM EvalHandler object.
#' @param targets Structured DWM targets.
#' @param adjusted Use adjusted source fields.
#' @param sparse Return only observed domain combinations.
#' @return A lazy plot-level query.
#' @keywords internal
.make_dwm_aggregates <- function(
  object,
  targets,
  adjusted = FALSE,
  sparse = FALSE
) {
  if (!inherits(object@spec, "DWMAnalysis")) {
    stop("DWM aggregation requires a `dwm_analysis()` handler.", call. = FALSE)
  }
  if (length(targets) == 0 || !all(vapply(
    targets,
    inherits,
    logical(1),
    what = "dwm_target"
  ))) {
    stop("At least one valid DWM component target is required.", call. = FALSE)
  }

  res <- .build_dwm_data(object)
  plot_keys <- .plot_keys_raw
  plot_domains <- .resolve_partition_domains(
    .pipeline_domains(object, "plot"),
    "plot",
    colnames(res)
  )
  cond_domains <- .resolve_partition_domains(
    .pipeline_domains(object, "cond"),
    "cond",
    colnames(res)
  )
  dwm_domains <- .resolve_partition_domains(
    .pipeline_domains(object, "dwm"),
    "dwm",
    colnames(res)
  )

  if (length(plot_domains) > 0) {
    res <- res %>% dplyr::group_by(!!!plot_domains)
  }
  if (length(cond_domains) > 0) {
    res <- res %>% dplyr::group_by(!!!cond_domains, .add = TRUE)
  }
  if (length(dwm_domains) > 0) {
    res <- res %>% dplyr::group_by(!!!dwm_domains, .add = TRUE)
  }
  res <- .group_by_missing_vars(res, plot_keys)

  output_names <- vapply(targets, function(target) target$name, character(1))
  if (anyDuplicated(output_names)) {
    stop("DWM target output names must be unique.", call. = FALSE)
  }

  source_columns <- lapply(targets, .resolve_dwm_columns, adjusted = adjusted)
  missing_columns <- setdiff(unique(unlist(source_columns)), colnames(res))
  if (length(missing_columns) > 0) {
    stop(
      "`COND_DWM_CALC` is missing required DWM column(s): ",
      paste(missing_columns, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  agg_exprs <- lapply(targets, function(target) {
    agg_expr(target, adjusted = adjusted)
  })
  names(agg_exprs) <- output_names

  aggregated <- res %>%
    dplyr::summarise(!!!agg_exprs) %>%
    dplyr::ungroup()

  domain_vars <- setdiff(dplyr::group_vars(res), plot_keys)
  .complete_scaffold(
    plot_qry = .build_plot_data(object),
    aggregated_qry = aggregated,
    plot_keys = plot_keys,
    domain_vars = domain_vars,
    sparse = sparse,
    zero_fill_vars = output_names
  ) %>%
    dplyr::rename(PLT_CN = CN)
}

#' Prepare plot-level data with mutations and filters applied
#'
#' @keywords internal
.build_plot_data <- function(object) {
  res <- object@tables$plot

  res <- .apply_level_pipeline(res, object, "plot")

  return(res)
}

#' Aggregate tree data to plot or subplot levels. This provides an unadjusted
#' density for the plot.
#'
#' @param object A EvalHandler object.
#' @param ... Variables to aggregate (tidy-select supported)
#' @param level The level to aggregate to. Can be "plot" or "subplot".
#' @keywords internal
.make_tree_aggregates <- function(
  object,
  ...,
  adjusted = FALSE,
  level = "plot",
  sparse = FALSE
) {
  res <- .build_tree_data(object)
  res <- res %>%
    dplyr::mutate(.expander_wt = TPA_UNADJ)

  if (adjusted) {
    res <- res %>%
      dplyr::left_join(
        object@tables$pop_plot_stratum_assgn %>%
          dplyr::select(PLT_CN, STRATUM_CN),
        by = c("CN" = "PLT_CN")
      ) %>%
      dplyr::left_join(
        object@tables$pop_stratum %>%
          dplyr::select(
            CN,
            ADJ_FACTOR_MACR,
            ADJ_FACTOR_MICR,
            ADJ_FACTOR_SUBP
          ) %>%
          dplyr::rename(STRATUM_CN = CN),
        by = "STRATUM_CN"
      )

    if (!"MACRO_BREAKPOINT_DIA" %in% colnames(res)) {
      res <- res %>% dplyr::mutate(MACRO_BREAKPOINT_DIA = NA_real_)
    }

    res <- res %>% dplyr::mutate(.adj_factor = !!get_adj_factor_expr())
  } else {
    res <- res %>% dplyr::mutate(.adj_factor = 1)
  }

  plot_keys <- .plot_keys_raw
  plot_domains <- .resolve_partition_domains(
    .pipeline_domains(object, "plot"),
    "plot",
    colnames(res)
  )
  cond_domains <- .resolve_partition_domains(
    .pipeline_domains(object, "cond"),
    "cond",
    colnames(res)
  )
  tree_domains <- .resolve_partition_domains(
    .pipeline_domains(object, "tree"),
    "tree",
    colnames(res)
  )

  # Determine target variables for aggregation
  target_vars <- dplyr::quos(...)

  if (.uses_default_expander_filter(target_vars)) {
    res <- res %>% dplyr::filter(!is.na(.expander_wt))
  }

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
    zero_fill_vars <- "tree_count"
  } else {
    resolved_names <- .resolve_tree_target_names(target_vars)
    zero_fill_vars <- character(0)

    agg_exprs <- purrr::map(seq_along(target_vars), function(i) {
      var_quo <- target_vars[[i]]
      expr <- rlang::quo_get_expr(var_quo)
      if (is.numeric(expr) && length(expr) == 1 && !is.na(expr) && expr == 1) {
        zero_fill_vars <<- c(zero_fill_vars, resolved_names[[i]])
        rlang::expr(sum(.expander_wt * .adj_factor, na.rm = TRUE))
      } else if (rlang::is_symbol(expr)) {
        zero_fill_vars <<- c(zero_fill_vars, resolved_names[[i]])
        rlang::expr(sum(.expander_wt * .adj_factor * (!!var_quo), na.rm = TRUE))
      } else {
        evaluated <- .eval_as_target(var_quo)
        if (inherits(evaluated, "fiaplyr_target")) {
          zero_fill_vars <<- c(zero_fill_vars, resolved_names[[i]])
          agg_expr(evaluated, adjusted = adjusted)
        } else {
          expr
        }
      }
    })
    names(agg_exprs) <- resolved_names

    aggregated <- res %>%
      dplyr::summarise(!!!agg_exprs) %>%
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
    sparse = sparse,
    zero_fill_vars = zero_fill_vars
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
#' @param adjusted Logical. If TRUE, applies stratum subplot adjustment
#'   factors based on GRM subtype code.
#' @param sparse Logical. If TRUE, returns a sparse result.
#' @keywords internal
.make_tree_history_aggregates <- function(
  object,
  ...,
  adjusted = FALSE,
  sparse = FALSE
) {
  res <- .build_tree_history_data(object)
  res <- res %>%
    dplyr::mutate(.expander_wt = TPA_UNADJ)

  if (adjusted) {
    subptype_adj_factors <- .get_subptype_adjustment_factors(object)

    res <- res %>%
      dplyr::left_join(
        object@tables$pop_plot_stratum_assgn %>%
          dplyr::select(PLT_CN, STRATUM_CN),
        by = c("CN" = "PLT_CN")
      ) %>%
      dplyr::mutate(
        ADJ_SUBPTYPE = dplyr::case_when(
          SUBPTYP_GRM == 1 ~ "SUBP",
          SUBPTYP_GRM == 2 ~ "MICR",
          SUBPTYP_GRM == 3 ~ "MACR",
          TRUE ~ NA_character_
        )
      ) %>%
      dplyr::left_join(
        subptype_adj_factors,
        by = c("STRATUM_CN", "ADJ_SUBPTYPE" = "SUBPTYPE")
      ) %>%
      dplyr::mutate(
        .expander_wt = dplyr::if_else(
          is.na(.expander_wt),
          NA_real_,
          .expander_wt * dplyr::coalesce(ADJ_FACTOR, 0)
        )
      )
  }

  plot_keys <- .plot_keys_raw
  plot_domains <- .resolve_partition_domains(
    .pipeline_domains(object, "plot"),
    "plot",
    colnames(res)
  )
  cond_domains <- .resolve_partition_domains(
    .pipeline_domains(object, "cond"),
    "cond",
    colnames(res)
  )
  tree_history_domains <- .resolve_partition_domains(
    .pipeline_domains(object, "tree_history"),
    "tree_history",
    colnames(res)
  )

  # Determine target variables for aggregation
  target_vars <- dplyr::quos(...)

  if (.uses_default_expander_filter(target_vars)) {
    res <- res %>% dplyr::filter(!is.na(.expander_wt))
  }

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
    zero_fill_vars <- "tree_count"
  } else {
    resolved_names <- .resolve_tree_target_names(target_vars)
    zero_fill_vars <- character(0)

    agg_exprs <- purrr::map(seq_along(target_vars), function(i) {
      var_quo <- target_vars[[i]]
      expr <- rlang::quo_get_expr(var_quo)
      if (is.numeric(expr) && length(expr) == 1 && !is.na(expr) && expr == 1) {
        zero_fill_vars <<- c(zero_fill_vars, resolved_names[[i]])
        rlang::expr(sum(.expander_wt, na.rm = TRUE))
      } else if (rlang::is_symbol(expr)) {
        zero_fill_vars <<- c(zero_fill_vars, resolved_names[[i]])
        rlang::expr(sum(.expander_wt * (!!var_quo), na.rm = TRUE))
      } else {
        evaluated <- .eval_as_target(var_quo)
        if (inherits(evaluated, "fiaplyr_target")) {
          zero_fill_vars <<- c(zero_fill_vars, resolved_names[[i]])
          agg_expr(evaluated, adjusted = adjusted)
        } else {
          expr
        }
      }
    })
    names(agg_exprs) <- resolved_names

    aggregated <- res %>%
      dplyr::summarise(!!!agg_exprs) %>%
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
    sparse = sparse,
    zero_fill_vars = zero_fill_vars
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
    dplyr::inner_join(
      object@tables$cond,
      by = c("CN" = "PLT_CN"),
      suffix = c("", ".cond")
    ) %>%
    dplyr::inner_join(
      object@tables$tree,
      by = c("CN" = "PLT_CN", "CONDID" = "CONDID"),
      suffix = c("", ".tree")
    )

  # Join to reference species table if available
  if (!is.null(object@tables$ref_species)) {
    res <- res %>%
      dplyr::left_join(
        object@tables$ref_species,
        by = "SPCD",
        suffix = c("", ".ref")
      )
  } else {
    warning(
      "REF_SPECIES table not available; continuing without species reference columns.",
      call. = FALSE
    )
  }

  res <- .apply_level_pipeline(res, object, "cond")
  res <- .apply_level_pipeline(res, object, "tree")

  return(res)
}

#' @keywords internal
.build_tree_history_data <- function(object) {
  # Start from prepared plot data (includes plot mutations/filters)
  res <- .build_plot_data(object)

  # Join to condition table, then tree_history table
  res <- res %>%
    dplyr::inner_join(
      object@tables$cond,
      by = c("CN" = "PLT_CN"),
      suffix = c("", ".cond")
    ) %>%
    dplyr::inner_join(
      object@tables$tree_history,
      by = c("CN" = "PLT_CN", "CONDID" = "CONDID"),
      suffix = c("", ".tree_history")
    )

  # Join to reference species table if available
  if (!is.null(object@tables$ref_species)) {
    res <- res %>%
      dplyr::left_join(
        object@tables$ref_species,
        by = "SPCD",
        suffix = c("", ".ref")
      )
  } else {
    warning(
      "REF_SPECIES table not available; continuing without species reference columns.",
      call. = FALSE
    )
  }

  res <- .apply_level_pipeline(res, object, "cond")
  res <- .apply_level_pipeline(res, object, "tree_history")

  return(res)
}
