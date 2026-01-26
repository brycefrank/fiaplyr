#' Class for Evaluation Pipeline
#'
#' @slot evalid The evaluation ID (numeric).
#' @slot plot Lazy query for PLOT table.
#' @slot tree Lazy query for TREE table.
#' @slot cond Lazy query for COND table.
#' @slot pop_estn_unit Lazy query for POP_ESTN_UNIT table.
#' @slot pop_stratum Lazy query for POP_STRATUM table.
#' @slot pop_plot_stratum_assgn Lazy query for POP_PLOT_STRATUM_ASSGN table.
#' @slot subp_cond Lazy query for SUBP_COND table.
#' @slot internal_cache Environment for caching intermediate results.
#' @export
setClass("EvalHandler",
  contains = "BaseHandler",
  slots = list(
    evalid = "numeric",
    plot = "ANY",
    tree = "ANY",
    cond = "ANY",
    pop_estn_unit = "ANY",
    pop_stratum = "ANY",
    pop_plot_stratum_assgn = "ANY",
    subp_cond = "ANY",
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
#' @export
eval_handler <- function(db, evalid) {
  pop_eval_qry <- dplyr::tbl(db, "POP_EVAL") %>%
    dplyr::filter(EVALID == !!evalid)

  # Filter POP_ESTN_UNIT first using EVALID
  pop_estn_unit_qry <- dplyr::tbl(db, "POP_ESTN_UNIT") %>%
    dplyr::semi_join(pop_eval_qry, by = c("EVAL_CN" = "CN"))

  # Filter POP_STRATUM using POP_ESTN_UNIT
  pop_stratum_qry <- dplyr::tbl(db, "POP_STRATUM") %>%
    dplyr::semi_join(pop_estn_unit_qry, by = c("ESTN_UNIT_CN" = "CN"))

  # Filter POP_PLOT_STRATUM_ASSGN using POP_STRATUM
  pop_plot_stratum_assgn_qry <- dplyr::tbl(db, "POP_PLOT_STRATUM_ASSGN") %>%
    dplyr::semi_join(pop_stratum_qry, by = c("STRATUM_CN" = "CN"))

  # Filter PLOT using POP_PLOT_STRATUM_ASSGN
  plot_qry <- dplyr::tbl(db, "PLOT") %>%
    dplyr::semi_join(pop_plot_stratum_assgn_qry, by = c("CN" = "PLT_CN"))

  # Filter COND using PLOT
  cond_qry <- dplyr::tbl(db, "COND") %>%
    dplyr::semi_join(plot_qry, by = c("PLT_CN" = "CN"))

  # Filter TREE using COND
  tree_qry <- dplyr::tbl(db, "TREE") %>%
    dplyr::semi_join(cond_qry, by = c("PLT_CN", "CONDID"))

  # Filter SUBP_COND using COND
  subp_cond_qry <- dplyr::tbl(db, "SUBP_COND") %>%
    dplyr::semi_join(cond_qry, by = c("PLT_CN", "CONDID"))

  new("EvalHandler",
    db = db,
    evalid = evalid,
    plot = plot_qry,
    tree = tree_qry,
    cond = cond_qry,
    pop_estn_unit = pop_estn_unit_qry,
    pop_stratum = pop_stratum_qry,
    pop_plot_stratum_assgn = pop_plot_stratum_assgn_qry,
    subp_cond = subp_cond_qry,
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

  # Eval Description
  eval_descr <- dplyr::tbl(object@db, "POP_EVAL") %>%
    dplyr::filter(EVALID == !!object@evalid) %>%
    dplyr::select(EVAL_DESCR) %>%
    dplyr::collect() %>%
    dplyr::pull(EVAL_DESCR)

  if (length(eval_descr) == 0) eval_descr <- NA_character_

  # Estimation Unit count
  n_estn_units <- object@pop_estn_unit %>%
    dplyr::tally() %>%
    dplyr::collect() %>%
    dplyr::pull(n)

  # Strata count
  n_strata <- object@pop_stratum %>%
    dplyr::tally() %>%
    dplyr::collect() %>%
    dplyr::pull(n)

  # Plot stats
  plot_stats <- object@plot %>%
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
    n_estn_units = n_estn_units,
    n_strata = n_strata,
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
  cat("Estn Units:     ", s$n_estn_units, "\n")
  cat("Strata:         ", s$n_strata, "\n")
  cat("Plots:          ", s$n_plots, "\n")
  cat("Inventory Years:", s$min_invyr, "-", s$max_invyr, "\n")
  cat("Measure Years:  ", s$min_meas, "-", s$max_meas, "\n")
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

#' Set Tree Domains (Grouping)
#'
#' @param .data A EvalHandler object.
#' @param ... Variables to group by.
#' @return A EvalHandler object with pending grouping.
#' @importFrom dplyr group_by
#' @export
setMethod("set_tree_domains", "EvalHandler", function(.data, ...) {
  # Capture expressions as quosures
  new_groups <- dplyr::quos(...)

  # Overwrite existing grouping
  .data@tree_domains <- new_groups

  return(.data)
})

#' Set Condition Domains (Grouping)
#'
#' @param .data A EvalHandler object.
#' @param ... Variables to group by.
#' @return A EvalHandler object with pending grouping.
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

#' Aggregate Trees to Plot Level
#'
#' @param object A EvalHandler object.
#' @param ... Variables to aggregate (tidy-select supported)
#' @return A lazy query with plot-level summaries.
#' @export
setMethod("aggregate_tree", "EvalHandler", function(object, ...) {
  .make_tree_aggregates(object, ...)
})

#' Aggregate Conditions to Plot Level
#'
#' @param object A EvalHandler object.
#' @param ... Variables to aggregate (tidy-select supported)
#' @return A lazy query with plot-level summaries.
#' @export
setMethod("aggregate_cond", "EvalHandler", function(object) {
  .make_cond_aggregates(object)
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
