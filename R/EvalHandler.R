#' Class for Evaluation Pipeline
#'
#' @slot evalid The evaluation ID (numeric).
#' @slot tables A list of lazy queries for the tables.
#' @slot schema The AnalysisSchema used.
#' @slot internal_cache Environment for caching intermediate results.
#' @export
setClass("EvalHandler",
  contains = "BaseHandler",
  slots = list(
    evalid = "numeric",
    tables = "list",
    schema = "AnalysisSchema",
    internal_cache = "environment",
    tree_mutations = "list",
    cond_mutations = "list",
    tree_domains = "ANY",
    cond_domains = "ANY",
    tree_filters = "list",
    cond_filters = "list"
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
#' @param schema An [AnalysisSchema][AnalysisSchema-class] object. Defaults to [StatusAnalysis][StatusAnalysis-class].
#' @param backend Optional DatabaseBackend for custom schema/table names.
#'
#' @return An object of class [EvalHandler][EvalHandler-class] connected to the specified evaluation.
#' @export
#'
#' @examples
#' # Connect to an evaluation with evalid 500601
#' con <- DBI::dbConnect(duckdb::duckdb(), fiadb_vt_mini_path())
#' handler <- eval_handler(con, evalid = 500601)
eval_handler <- function(db, evalid, schema = new("StatusAnalysis"), backend = NULL) {
  tables <- initialize_tables(schema, db, evalid, backend)

  if (!is.null(tables$pop_eval)) {
    if (tables$pop_eval %>% dplyr::tally() %>% dplyr::collect() %>% dplyr::pull(n) == 0) {
      stop(paste0("EVALID ", evalid, " does not exist in the database."))
    }
  }

  new("EvalHandler",
    db = db,
    evalid = evalid,
    tables = tables,
    schema = schema,
    internal_cache = new.env(parent = emptyenv()),
    tree_mutations = list(),
    cond_mutations = list(),
    tree_domains = list(),
    cond_domains = list(),
    tree_filters = list(),
    cond_filters = list()
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

  # Plot stats
  plot_stats <- object@tables$plot %>%
    dplyr::summarise(
      n_plots = dplyr::n(),
      min_invyr = min(INVYR, na.rm = TRUE),
      max_invyr = max(INVYR, na.rm = TRUE),
      min_meas = min(MEASYEAR, na.rm = TRUE),
      max_meas = max(MEASYEAR, na.rm = TRUE)
    ) %>%
    dplyr::collect()

  res <- list(
    eval_descr = eval_descr,
    n_plots = plot_stats$n_plots,
    min_invyr = plot_stats$min_invyr,
    max_invyr = plot_stats$max_invyr,
    min_meas = plot_stats$min_meas,
    max_meas = plot_stats$max_meas
  )

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
  cat("Inventory Years:", s$min_invyr, "-", s$max_invyr, "\n")
  cat("Measure Years:  ", s$min_meas, "-", s$max_meas, "\n")

  # Display domain variables if set
  tree_dom_labels <- vapply(object@tree_domains, rlang::as_label, character(1))
  cond_dom_labels <- vapply(object@cond_domains, rlang::as_label, character(1))

  if (length(tree_dom_labels) > 0 || length(cond_dom_labels) > 0) {
    cat("\n")
    if (length(tree_dom_labels) > 0) {
      cat("Tree domains:   ", paste(tree_dom_labels, collapse = ", "), "\n")
    }
    if (length(cond_dom_labels) > 0) {
      cat("Cond domains:   ", paste(cond_dom_labels, collapse = ", "), "\n")
    }
  }
})

#' Mutate Tree Table
#'
#' @param handler A EvalHandler object.
#' @param ... Name-value pairs of expressions.
#' @return A EvalHandler object with pending mutations.
#' @export
setMethod("mutate_tree", "EvalHandler", function(handler, ...) {
  # Capture expressions as quosures
  new_mutations <- dplyr::quos(...)

  # Append to existing mutations
  handler@tree_mutations <- c(handler@tree_mutations, new_mutations)

  return(handler)
})

#' Mutate Condition Table
#'
#' @param handler A EvalHandler object.
#' @param ... Name-value pairs of expressions.
#' @return A EvalHandler object with pending mutations.
#' @export
setMethod("mutate_cond", "EvalHandler", function(handler, ...) {
  # Capture expressions as quosures
  new_mutations <- dplyr::quos(...)

  # Append to existing mutations
  handler@cond_mutations <- c(handler@cond_mutations, new_mutations)

  return(handler)
})

#' Set Tree Domain Variables
#'
#' Sets the domain variables used for grouping tree-level aggregations.
#' A domain variable is a column (e.g., STATUSCD, SPCD) whose unique
#' values or combinations define estimation domains.
#'
#' @param .data A EvalHandler object.
#' @param ... Domain variable names (unquoted column names).
#' @return A EvalHandler object with the tree domain variables set.
#' @export
setMethod("set_tree_domains", "EvalHandler", function(.data, ...) {
  # Capture expressions as quosures
  new_groups <- dplyr::quos(...)

  # Overwrite existing grouping
  .data@tree_domains <- new_groups

  return(.data)
})

#' Set Condition Domain Variables
#'
#' Sets the domain variables used for grouping condition-level aggregations.
#' A domain variable is a column (e.g., FORTYPCD, OWNGRPCD) whose unique
#' values or combinations define estimation domains.
#'
#' @param .data A EvalHandler object.
#' @param ... Domain variable names (unquoted column names).
#' @return A EvalHandler object with the condition domain variables set.
#' @export
setMethod("set_cond_domains", "EvalHandler", function(.data, ...) {
  # Capture expressions as quosures
  new_groups <- dplyr::quos(...)

  # Overwrite existing grouping
  .data@cond_domains <- new_groups

  return(.data)
})

#' Filter the Tree Table
#'
#' This function applies filters to the tree table. This is more complex than
#' a standard `dplyr::filter()` because filters are applied lazily in tandem
#' with other pre-joined tables (e.g., `REF_SPECIES`). However, the usage and
#' interpretation is much the same, conditional statements are provided and
#' tree records that do not satisfy the conditions will be excluded from all
#' subsequent operations, including aggregations and estimates.
#'
#' @param handler An [EvalHandler][EvalHandler-class] object.
#' @param ... Logical predicates defined in terms of the variables in the tree
#'  table.
#' @return An [EvalHandler][EvalHandler-class] object with pending filters.
#' @export
#'
#' @examples
#' handler <- eval_handler(con, evalid = 500601) |>
#'  filter_tree(STATUSCD == 1) # Only include live trees
setMethod("filter_tree", "EvalHandler", function(handler, ...) {
  new_filters <- dplyr::quos(...)
  handler@tree_filters <- c(handler@tree_filters, new_filters)

  return(handler)
})

#' Filter the Condition Table
#'
#' This function applies filters to the condition table. This is more complex
#' than a standard `dplyr::filter()` because filters are applied lazily in
#' tandem with other pre-joined tables. For example, filtering to a specific
#' `OWNGRPCD` will exclude all conditions *and* all trees that do not satisfy
#' that condition, which will impact all subsequent operations.
#'
#' @param handler An [EvalHandler][EvalHandler-class] object.
#' @param ... Logical predicates defined in terms of the variables in the
#'  condition table.
#' @return An [EvalHandler][EvalHandler-class] object with pending filters.
#' @export
#'
#' @examples
#' handler <- eval_handler(con, evalid = 500601) |>
#'  filter_cond(OWNGRPCD == 10)
setMethod("filter_cond", "EvalHandler", function(handler, ...) {
  new_filters <- dplyr::quos(...)
  handler@cond_filters <- c(handler@cond_filters, new_filters)

  return(handler)
})

#' Aggregate Data to the Plot Level
#'
#' @param handler A EvalHandler object.
#' @param ... A formula specifying the aggregation target (e.g., tree ~ VOLCFGRS), and optional arguments like `sparse`.
#' @return A lazy query with plot-level summaries.
#' @export
setMethod("aggregate", "EvalHandler", function(handler, ...) {
  aggregate_data(handler@schema, handler, ...)
})

#' Aggregate Trees to Plot Level
#'
#' @param object A EvalHandler object.
#' @param ... Variables to aggregate (tidy-select supported)
#' @param sparse Logical. If TRUE, returns a sparse result (only observed combinations). Defaults to FALSE.
#' @return A lazy query with plot-level summaries.
#' @export
setMethod("aggregate_tree", "EvalHandler", function(object, ..., sparse = FALSE) {
  .Deprecated("aggregate")
  .make_tree_aggregates(object, ..., sparse = sparse)
})

#' Aggregate Conditions to Plot Level
#'
#' @param object A EvalHandler object.
#' @param sparse Logical. If TRUE, returns a sparse result (only observed combinations). Defaults to FALSE.
#' @return A lazy query with plot-level summaries.
#' @export
setMethod("aggregate_cond", "EvalHandler", function(object, sparse = FALSE) {
  .Deprecated("aggregate")
  .make_cond_aggregates(object, sparse = sparse)
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
#' @return The evaluation ID.
#' @export
setGeneric("evalid", function(handler) standardGeneric("evalid"))

#' @describeIn evalid Get evaluation ID for EvalHandler
setMethod("evalid", "EvalHandler", function(handler) {
  handler@evalid
})

#' Set Evaluation ID
#'
##' @param handler A EvalHandler object.
#' @param value The new evaluation ID.
#' @return The modified object.
#' @export
setGeneric("evalid<-", function(handler, value) standardGeneric("evalid<-"))

##' @describeIn evalid Set evaluation ID for EvalHandler
setMethod("evalid<-", "EvalHandler", function(handler, value) {
  handler@evalid <- value
  handler
})
