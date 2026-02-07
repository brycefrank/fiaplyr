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

#' Constructor for EvalHandler
#'
#' @param db A DBIConnection object.
#' @param evalid A numeric identifier for the evaluation.
#' @param schema An AnalysisSchema object. Defaults to StatusAnalysis.
#' @param backend Optional DatabaseBackend for custom schema/table names.
#' @export
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
#' @param .data A EvalHandler object.
#' @param ... Name-value pairs of expressions.
#' @return A EvalHandler object with pending mutations.
#' @importFrom dplyr mutate
#' @export
setMethod("mutate_tree", "EvalHandler", function(.data, ...) {
  # Capture expressions as quosures
  new_mutations <- dplyr::quos(...)

  # Append to existing mutations
  .data@tree_mutations <- c(.data@tree_mutations, new_mutations)

  return(.data)
})

#' Mutate Condition Table
#'
#' @param .data A EvalHandler object.
#' @param ... Name-value pairs of expressions.
#' @return A EvalHandler object with pending mutations.
#' @importFrom dplyr mutate
#' @export
setMethod("mutate_cond", "EvalHandler", function(.data, ...) {
  # Capture expressions as quosures
  new_mutations <- dplyr::quos(...)

  # Append to existing mutations
  .data@cond_mutations <- c(.data@cond_mutations, new_mutations)

  return(.data)
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
#' @importFrom dplyr group_by
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
#' @importFrom dplyr group_by
#' @export
setMethod("set_cond_domains", "EvalHandler", function(.data, ...) {
  # Capture expressions as quosures
  new_groups <- dplyr::quos(...)

  # Overwrite existing grouping
  .data@cond_domains <- new_groups

  return(.data)
})

#' Filter Tree Table
#'
#' @param .data A EvalHandler object.
#' @param ... Logical predicates defined in terms of the variables in the tree table.
#' @return A EvalHandler object with pending filters.
#' @importFrom dplyr filter
#' @export
setMethod("filter_tree", "EvalHandler", function(.data, ...) {
  # Capture expressions as quosures
  new_filters <- dplyr::quos(...)

  # Append to existing filters
  .data@tree_filters <- c(.data@tree_filters, new_filters)

  return(.data)
})

#' Filter Condition Table
#'
#' @param .data A EvalHandler object.
#' @param ... Logical predicates defined in terms of the variables in the condition table.
#' @return A EvalHandler object with pending filters.
#' @importFrom dplyr filter
#' @export
setMethod("filter_cond", "EvalHandler", function(.data, ...) {
  # Capture expressions as quosures
  new_filters <- dplyr::quos(...)

  # Append to existing filters
  .data@cond_filters <- c(.data@cond_filters, new_filters)

  return(.data)
})

#' Aggregate Data to Plot Level
#'
#' @param x A EvalHandler object.
#' @param ... A formula specifying the aggregation target (e.g., tree ~ VOLCFGRS), and optional arguments like `sparse`.
#' @return A lazy query with plot-level summaries.
#' @export
setMethod("aggregate", "EvalHandler", function(x, ...) {
  aggregate_data(x@schema, x, ...)
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
setMethod("get_strata_weights", "EvalHandler", function(object) {
  object@tables$pop_stratum %>%
    dplyr::inner_join(
      object@tables$pop_estn_unit,
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
#' @param object A EvalHandler object.
#' @return The evaluation ID.
#' @export
setGeneric("evalid", function(object) standardGeneric("evalid"))

#' @describeIn evalid Get evaluation ID for EvalHandler
setMethod("evalid", "EvalHandler", function(object) {
  object@evalid
})

#' Set Evaluation ID
#'
#' @param object A EvalHandler object.
#' @param value The new evaluation ID.
#' @return The modified object.
#' @export
setGeneric("evalid<-", function(object, value) standardGeneric("evalid<-"))

#' @describeIn evalid Set evaluation ID for EvalHandler
setMethod("evalid<-", "EvalHandler", function(object, value) {
  object@evalid <- value
  object
})
