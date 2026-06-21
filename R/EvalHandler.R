#' Class for Evaluation Pipeline
#'
#' @slot evalid The evaluation ID (numeric).
#' @slot plot_mutations Pending plot-level mutation quosures.
#' @slot plot_filters Pending plot-level filter quosures.
#' @slot plot_domains Pending plot-level domain quosures.
#' @slot tree_mutations Pending tree-level mutation quosures.
#' @slot cond_mutations Pending condition-level mutation quosures.
#' @slot tree_history_mutations Pending tree-history-level mutation quosures.
#' @slot tree_domains Pending tree-level domain quosures.
#' @slot cond_domains Pending condition-level domain quosures.
#' @slot tree_history_domains Pending tree-history-level domain quosures.
#' @slot tree_filters Pending tree-level filter quosures.
#' @slot cond_filters Pending condition-level filter quosures.
#' @slot tree_history_filters Pending tree-history-level filter quosures.
#' @slot tables A list of lazy queries for the tables.
#' @slot spec The AnalysisSpec used.
#' @slot internal_cache Environment for caching intermediate results.
#' @export
setClass("EvalHandler",
  contains = "BaseHandler",
  slots = list(
    evalid = "numeric",
    tables = "list",
    spec = "AnalysisSpec",
    internal_cache = "environment",
    plot_mutations = "list",
    plot_filters = "list",
    plot_domains = "ANY",
    tree_mutations = "list",
    cond_mutations = "list",
    tree_history_mutations = "list",
    tree_domains = "ANY",
    cond_domains = "ANY",
    tree_history_domains = "ANY",
    tree_filters = "list",
    cond_filters = "list",
    tree_history_filters = "list"
  )
)

#' Connect to an Evaluation
#'
#' In FIA parlance, an evaluation specifies an area (usually a state), a time
#' window, a set of plots, and an associated post-stratification. The
#' `eval_handler()` function connects to an evaluation and allows users to
#' manipulate the underlying data to produce estimates and aggregates of need.
#' Refer to [Status Estimates](/guides/status_estimates/) for an introduction.
#' 
#' @param db A DBIConnection object.
#' @param evalid A numeric identifier for the evaluation.
#' @param spec An [AnalysisSpec][AnalysisSpec-class] object. Defaults to `new("StatusAnalysis")`.
#' @param backend Optional DatabaseMapping for custom schema/table names.
#'
#' @return An object of class [EvalHandler][EvalHandler-class] connected to the specified evaluation.
#' @export
#'
#' @examples
#' if (requireNamespace("duckdb", quietly = TRUE)) {
#'   con <- DBI::dbConnect(duckdb::duckdb(), fiadb_vt_mini_path())
#'   handler <- eval_handler(con, evalid = 500601)
#'   DBI::dbDisconnect(con, shutdown = TRUE)
#' }
eval_handler <- function(db, evalid, spec = new("StatusAnalysis"), backend = NULL) {
  tables <- initialize_tables(spec, db, evalid, backend)

  if (!is.null(tables$pop_eval)) {
    if (tables$pop_eval %>% dplyr::tally() %>% dplyr::collect() %>% dplyr::pull(n) == 0) {
      stop(paste0("EVALID ", evalid, " does not exist in the database."))
    }
  }

  new("EvalHandler",
    db = db,
    evalid = evalid,
    tables = tables,
    spec = spec,
    internal_cache = new.env(parent = emptyenv()),
    plot_mutations = list(),
    plot_filters = list(),
    plot_domains = list(),
    tree_mutations = list(),
    cond_mutations = list(),
    tree_history_mutations = list(),
    tree_domains = list(),
    cond_domains = list(),
    tree_history_domains = list(),
    tree_filters = list(),
    cond_filters = list(),
    tree_history_filters = list()
  )
}

#' Summary Method for EvalHandler
#'
#' @param object A EvalHandler object.
#' @export
setMethod("summary", "EvalHandler", function(object) {
  # Check cache
  if (!is.null(object@internal_cache$summary)) {
    return(object@internal_cache$summary)
  }

  # Eval Description - use the already-loaded table instead of hardcoded name
  eval_descr <- object@tables$pop_eval %>%
    dplyr::select(EVAL_DESCR) %>%
    dplyr::collect() %>%
    dplyr::pull(EVAL_DESCR)

  if (length(eval_descr) == 0) eval_descr <- NA_character_

  n_plots <- object@tables$plot %>%
    dplyr::summarise(n_plots = dplyr::n()) %>%
    dplyr::collect() %>%
    dplyr::pull(n_plots)

  spec_fields <- spec_summary_fields(object@spec, object)

  res <- c(list(
    eval_descr = eval_descr,
    n_plots = n_plots
  ), spec_fields)

  # Populate cache
  object@internal_cache$summary <- res

  return(res)
})

#' Show Method for EvalHandler
#'
#' @param object A EvalHandler object.
#' @export
setMethod("show", "EvalHandler", function(object) {
  cat("EvalHandler\n")
  cat("----------\n")

  s <- summary(object)

  cat("EVALID:         ", object@evalid, "\n")

  # Formatted Description
  descr_label <- "Description:     "
  descr_text <- if (is.na(s$eval_descr)) "NA" else s$eval_descr
  wrapped_descr <- strwrap(descr_text, width = 60, indent = 0, exdent = 0)

  cat(paste0(descr_label, wrapped_descr[1], "\n"))
  if (length(wrapped_descr) > 1) {
    indent_space <- paste(rep(" ", nchar(descr_label)), collapse = "")
    for (i in 2:length(wrapped_descr)) {
      cat(paste0(indent_space, wrapped_descr[i], "\n"))
    }
  }

  cat("\n")
  cat("Plots:          ", s$n_plots, "\n")

  if (all(c("min_invyr", "max_invyr") %in% names(s))) {
    cat("Inventory Years:", s$min_invyr, "-", s$max_invyr, "\n")
  }
  if (all(c("min_meas", "max_meas") %in% names(s))) {
    cat("Measure Years:  ", s$min_meas, "-", s$max_meas, "\n")
  }

  # Display domain variables if set
  plot_dom_labels <- vapply(object@plot_domains, rlang::as_label, character(1))
  tree_dom_labels <- vapply(object@tree_domains, rlang::as_label, character(1))
  cond_dom_labels <- vapply(object@cond_domains, rlang::as_label, character(1))

  if (length(plot_dom_labels) > 0 || length(tree_dom_labels) > 0 || length(cond_dom_labels) > 0) {
    cat("\n")
    if (length(plot_dom_labels) > 0) {
      cat("Plot domains:   ", paste(plot_dom_labels, collapse = ", "), "\n")
    }
    if (length(tree_dom_labels) > 0) {
      cat("Tree domains:   ", paste(tree_dom_labels, collapse = ", "), "\n")
    }
    if (length(cond_dom_labels) > 0) {
      cat("Cond domains:   ", paste(cond_dom_labels, collapse = ", "), "\n")
    }
  }
})

#' Internal: Route and Queue Scoped Expressions
#'
#' Validates that all arguments are scoped via helpers and routes them to the
#' appropriate handler slots (mutations, filters, or domains).
#'
#' @param handler An EvalHandler object.
#' @param args Evaluated arguments from `rlang::list2(...)`.
#' @param operation One of "append_mutations", "append_filters", or "set_domains".
#' @return The modified handler.
#' @keywords internal
.route_scoped_expressions <- function(handler, args, operation = "append_mutations") {
  if (length(args) == 0) {
    return(handler)
  }

  # Check if the entire args list has a target_table attribute (from scoped helpers)
  list_target_table <- attr(args, "target_table")
  
  if (!is.null(list_target_table)) {
    # All expressions in this list share the same target
    if (!list_target_table %in% c("tree", "cond", "plot", "tree_history")) {
      rlang::abort(
        sprintf("Invalid target table: '%s'. Must be 'tree', 'cond', 'plot', or 'tree_history'.", list_target_table)
      )
    }
    
    slot_mutations <- paste0(list_target_table, "_mutations")
    slot_filters <- paste0(list_target_table, "_filters")
    slot_domains <- paste0(list_target_table, "_domains")
    args <- .expand_grm_helpers_scoped(args, list_target_table)
    
    if (operation == "append_mutations") {
      slot(handler, slot_mutations) <- c(slot(handler, slot_mutations), args)
    } else if (operation == "append_filters") {
      slot(handler, slot_filters) <- c(slot(handler, slot_filters), args)
    } else if (operation == "set_domains") {
      slot(handler, slot_domains) <- args
    }
  } else {
    domain_updates <- list(tree = NULL, cond = NULL, plot = NULL, tree_history = NULL)

    # Multiple expressions with potentially different targets
    for (arg in args) {
      if (is.null(arg)) next

      target_table <- attr(arg, "target_table")

      if (is.null(target_table)) {
        rlang::abort(
          "All expressions must be explicitly scoped using `tree()`, `cond()`, `plot()`, or `tree_history()`.",
          class = "fiaplyr_unscoped_expr"
        )
      }

      if (!target_table %in% c("tree", "cond", "plot", "tree_history")) {
        rlang::abort(
          sprintf("Invalid target table: '%s'. Must be 'tree', 'cond', 'plot', or 'tree_history'.", target_table)
        )
      }

      slot_mutations <- paste0(target_table, "_mutations")
      slot_filters <- paste0(target_table, "_filters")
      slot_domains <- paste0(target_table, "_domains")
      arg <- .expand_grm_helpers_scoped(arg, target_table)

      if (operation == "append_mutations") {
        slot(handler, slot_mutations) <- c(slot(handler, slot_mutations), arg)
      } else if (operation == "append_filters") {
        slot(handler, slot_filters) <- c(slot(handler, slot_filters), arg)
      } else if (operation == "set_domains") {
        domain_updates[[target_table]] <- c(domain_updates[[target_table]], arg)
      }
    }

    if (operation == "set_domains") {
      for (target_table in names(domain_updates)) {
        if (is.null(domain_updates[[target_table]])) next

        slot_domains <- paste0(target_table, "_domains")
        slot(handler, slot_domains) <- domain_updates[[target_table]]
      }
    }
  }
  
  handler
}

.has_scoped_helper_target <- function(arg) {
  is.list(arg) && !is.null(attr(arg, "target_table"))
}

.grm_helper_names <- c("grm_survivor", "grm_ingrowth_live", "grm_mortality")

.expand_grm_helpers_expr <- function(expr, env, target_table) {
  if (rlang::is_call(expr)) {
    fn_name <- rlang::call_name(expr)

    if (!is.null(fn_name) && fn_name %in% .grm_helper_names) {
      if (!identical(target_table, "tree_history")) {
        rlang::abort(
          paste0("`", fn_name, "()` can only be used inside `tree_history(...)`.")
        )
      }

      expanded <- rlang::eval_tidy(expr, env = env)
      if (rlang::is_quosure(expanded)) {
        expanded <- rlang::get_expr(expanded)
      }

      if (!rlang::is_call(expanded) && !rlang::is_symbol(expanded)) {
        rlang::abort(
          paste0(
            "`", fn_name, "()` must return an expression suitable for lazy SQL translation."
          )
        )
      }

      return(expanded)
    }

    call_parts <- as.list(expr)
    if (length(call_parts) > 1) {
      for (idx in 2:length(call_parts)) {
        call_parts[[idx]] <- .expand_grm_helpers_expr(call_parts[[idx]], env, target_table)
      }
    }

    return(as.call(call_parts))
  }

  expr
}

.expand_grm_helpers_quosure <- function(q, target_table) {
  if (!rlang::is_quosure(q)) {
    return(q)
  }

  new_expr <- .expand_grm_helpers_expr(
    expr = rlang::get_expr(q),
    env = rlang::get_env(q),
    target_table = target_table
  )

  rlang::new_quosure(new_expr, env = rlang::get_env(q))
}

.expand_grm_helpers_scoped <- function(scoped_args, target_table) {
  out <- lapply(scoped_args, .expand_grm_helpers_quosure, target_table = target_table)
  names(out) <- names(scoped_args)
  attr(out, "target_table") <- target_table
  out
}

.normalize_scoped_args <- function(args_list, quosures) {
  if (length(args_list) == 1 && .has_scoped_helper_target(args_list[[1]])) {
    return(args_list[[1]])
  }

  if (length(args_list) > 1 && all(vapply(args_list, .has_scoped_helper_target, logical(1)))) {
    return(args_list)
  }

  quosures
}

#' Transform: Add Derived Columns or Modify Values
#'
#' Add derived columns or modify existing ones at the plot, condition, or tree
#' level. Expressions must be wrapped in the appropriate scoping helper:
#' `tree()`, `cond()`, or `plot()`.
#'
#' @param handler An EvalHandler object.
#' @param ... Scoped expressions using `tree()`, `cond()`, or `plot()` helpers.
#' @return The handler with pending mutations queued.
#' @export
#' @examples
#' \dontrun{
#'   handler <- eval_handler(con, evalid = 500601)
#'   handler |>
#'     transform(tree(BA = 0.005454 * DIA^2))
#' }
setMethod("transform", "EvalHandler", function(handler, ...) {
  # Get arguments as list - they may be tagged quosure lists from helpers
  # or raw expressions that need to be captured
  args_list <- list(...)

  args <- .normalize_scoped_args(args_list, rlang::enquos(...))

  .route_scoped_expressions(handler, args, operation = "append_mutations")
})

#' Subset: Apply Scoped Filters
#'
#' Filter rows at the plot, condition, or tree level using logical predicates.
#' Expressions must be wrapped in the appropriate scoping helper:
#' `tree()`, `cond()`, or `plot()`. Rows that do not satisfy conditions are
#' excluded from all subsequent operations.
#'
#' @param handler An EvalHandler object.
#' @param ... Scoped logical expressions using `tree()`, `cond()`, or `plot()` helpers.
#' @return The handler with pending filters queued.
#' @export
#' @examples
#' \dontrun{
#'   handler <- eval_handler(con, evalid = 500601)
#'   handler |>
#'     subset(tree(STATUSCD == 1))
#' }
setMethod("subset", "EvalHandler", function(handler, ...) {
  # Get arguments as list - they may be tagged quosure lists from helpers
  # or raw expressions that need to be captured
  args_list <- list(...)

  args <- .normalize_scoped_args(args_list, rlang::enquos(...))

  .route_scoped_expressions(handler, args, operation = "append_filters")
})

#' Partition: Specify Domain Variables
#'
#' Set domain (grouping) variables at the plot, condition, or tree level.
#' Domain variables define how aggregation and estimation results are partitioned.
#' Unlike `subset()`, partitions do not discard data. Expressions must be wrapped
#' in the appropriate scoping helper: `tree()`, `cond()`, or `plot()`.
#'
#' @param handler An EvalHandler object.
#' @param ... Scoped domain variable names using `tree()`, `cond()`, or `plot()` helpers.
#' @return The handler with domain variables set.
#' @export
#' @examples
#' \dontrun{
#'   handler <- eval_handler(con, evalid = 500601)
#'   handler |>
#'     partition(tree(SPCD), cond(OWNCD))
#' }
setMethod("partition", "EvalHandler", function(handler, ...) {
  # Get arguments as list - they may be tagged quosure lists from helpers
  # or raw expressions that need to be captured
  args_list <- list(...)

  args <- .normalize_scoped_args(args_list, rlang::enquos(...))

  .route_scoped_expressions(handler, args, operation = "set_domains")
})

#' Mutate Tree Table (Deprecated)
#'
#' **Deprecated.** Use `transform(tree(...))` instead.
#'
#' @param handler A EvalHandler object.
#' @param ... Name-value pairs of expressions.
#' @return A EvalHandler object with pending mutations.
#' @export
setMethod("mutate_tree", "EvalHandler", function(handler, ...) {
  lifecycle::deprecate_warn(
    "0.1.0",
    "mutate_tree()",
    details = "Use `handler |> transform(tree(...))` instead of `handler |> mutate_tree(...)` to apply tree-level mutations."
  )

  # Capture expressions as quosures and wrap them in tree()
  new_mutations <- dplyr::quos(...)
  attr(new_mutations, "target_table") <- "tree"

  # Append to existing mutations
  handler@tree_mutations <- c(handler@tree_mutations, new_mutations)

  return(handler)
})

#' Mutate Condition Table (Deprecated)
#'
#' **Deprecated.** Use `transform(cond(...))` instead.
#'
#' @param handler A EvalHandler object.
#' @param ... Name-value pairs of expressions.
#' @return A EvalHandler object with pending mutations.
#' @export
setMethod("mutate_cond", "EvalHandler", function(handler, ...) {
  lifecycle::deprecate_warn(
    "0.1.0",
    "mutate_cond()",
    details = "Use `handler |> transform(cond(...))` instead of `handler |> mutate_cond(...)` to apply condition-level mutations."
  )

  # Capture expressions as quosures and wrap them in cond()
  new_mutations <- dplyr::quos(...)
  attr(new_mutations, "target_table") <- "cond"

  # Append to existing mutations
  handler@cond_mutations <- c(handler@cond_mutations, new_mutations)

  return(handler)
})

#' Aggregate a Handler to the Plot Level
#'
#' Aggregation generates plot (or subplot) level summaries of inventory
#' components
#'
#' @param handler A EvalHandler object.
#' @param ... A scoped target helper such as `tree(VOLCFGRS)` or `cond()`, and
#'   optional arguments like `sparse`.
#' @param expander Tree expansion column used when aggregating tree-level
#'   summaries (for example, `TPA_UNADJ`). Defaults to `TPA_UNADJ`.
#' @return A lazy query with plot-level summaries.
#' @export
setMethod("aggregate", "EvalHandler", function(handler, ..., expander = TPA_UNADJ) {
  expander_name <- rlang::as_name(rlang::ensym(expander))
  args <- list(...)
  args$expander <- expander_name
  do.call(aggregate_data, c(list(spec = handler@spec, handler = handler), args))
})

#' @describeIn get_strata_weights Get strata weights for EvalHandler
setMethod("get_strata_weights", "EvalHandler", function(handler) {
  handler@tables$pop_stratum %>%
    dplyr::inner_join(
      handler@tables$pop_estn_unit,
      by = c("ESTN_UNIT_CN" = "CN"),
      suffix = c("", ".eu")
    ) %>%
    dplyr::mutate(
      w_h = as.numeric(P1POINTCNT) / P1PNTCNT_EU
    ) %>%
    dplyr::select(
      STRATUM_CN = CN, ESTN_UNIT_CN, w_h, P2POINTCNT, AREA_USED
    )
})

#' Get Evaluation ID
#'
#' @param handler A EvalHandler object.
#' @param value The new evaluation ID.
#' @return The evaluation ID.
#' @export
setGeneric("evalid", function(handler) standardGeneric("evalid"))

#' @describeIn evalid Get evaluation ID for EvalHandler
setMethod("evalid", "EvalHandler", function(handler) {
  handler@evalid
})

#' Set Evaluation ID
#'
#' @param handler A EvalHandler object.
#' @param value The new evaluation ID.
#' @return The modified object.
#' @export
setGeneric("evalid<-", function(handler, value) standardGeneric("evalid<-"))

#' @describeIn evalid Set evaluation ID for EvalHandler
setMethod("evalid<-", "EvalHandler", function(handler, value) {
  handler@evalid <- value
  handler
})
