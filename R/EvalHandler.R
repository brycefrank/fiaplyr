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
    cond_domains = "ANY"
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
    cond_domains = list()
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

#' Specify Tree Domains (Grouping)
#'
#' @param .data A EvalHandler object.
#' @param ... Variables to group by.
#' @return A EvalHandler object with pending grouping.
#' @importFrom dplyr group_by
#' @export
setMethod("specify_tree_domains", "EvalHandler", function(.data, ...) {
  # Capture expressions as quosures
  new_groups <- dplyr::quos(...)
  
  # Overwrite existing grouping
  .data@tree_domains <- new_groups
  
  return(.data)
})

#' Specify Condition Domains (Grouping)
#'
#' @param .data A EvalHandler object.
#' @param ... Variables to group by.
#' @return A EvalHandler object with pending grouping.
#' @importFrom dplyr group_by
#' @export
setMethod("specify_cond_domains", "EvalHandler", function(.data, ...) {
  # Capture expressions as quosures
  new_groups <- dplyr::quos(...)
  
  # Overwrite existing grouping
  .data@cond_domains <- new_groups
  
  return(.data)
})

#' Aggregate condition data to plot or subplot levels
#'
#' @param object A EvalHandler object.
#' @param ... Variables to aggregate (tidy-select supported)
#' @param level The level to aggregate to. Can be "plot" or "subplot".
#' @keywords internal
.make_cond_aggregates <- function(object, ..., level = "plot", prop_basis = "MACR") {
  # Join core tables
  # PLOT -> COND
  
  # Start with PLOT
  res <- object@plot %>%
    dplyr::inner_join(object@cond, by = c("CN" = "PLT_CN"), suffix = c("", ".cond"))
  
  # Apply standard filtering
  res <- res %>%
    dplyr::filter(
      !is.na(CONDPROP_UNADJ)
    )
    
  if (!is.null(prop_basis)) {
    res <- res %>%
      dplyr::filter(PROP_BASIS == !!prop_basis)
  }
  
  # Apply pending mutations from the handler object
  if (length(object@cond_mutations) > 0) {
    res <- res %>% dplyr::mutate(!!!object@cond_mutations)
  }
  
  # Capture grouping variables
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

  # Check if specific variables are provided to aggregate
  agg_vars <- dplyr::quos(...)
  
  if (length(agg_vars) == 0) {
    # Default behavior: Just sum the condition proportion (divide by 4)
    # This gives the proportion of the plot covered by the condition domain
    aggregated <- res %>%
      dplyr::summarise(
        prop_area = sum(CONDPROP_UNADJ, na.rm = TRUE)
      ) %>%
      dplyr::ungroup()
      
  } else {
    # Weighted aggregation: Sum(Value * Prop / 4)
    aggregated <- res %>%
      dplyr::summarise(
        dplyr::across(c(...), ~ sum(.x * CONDPROP_UNADJ, na.rm = TRUE))
      ) %>%
      dplyr::ungroup()
  }

  # ----------------------------------------------------------------
  # SCAFFOLD CONSTRUCTION
  # ----------------------------------------------------------------
  
  # 1. Get all plots (Full Plot List)
  all_plots <- object@plot %>%
    dplyr::select(dplyr::all_of(plot_keys))
  
  # 2. Identify Domain Variables
  all_groups <- dplyr::group_vars(res)
  domain_vars <- setdiff(all_groups, plot_keys)
  
  # Construct the scaffold
  if (length(domain_vars) > 0) {
    # Extract distinct combinations of domain variables from the aggregated data
    observed_domains <- aggregated %>%
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

  # ----------------------------------------------------------------
  # FINAL MERGE
  # ----------------------------------------------------------------
  
  # Left join aggregated data onto the scaffold
  final_res <- scaffold %>%
    dplyr::left_join(aggregated, by = join_by)

  if (length(agg_vars) == 0) {
    final_res <- final_res %>%
      dplyr::mutate(prop_area = dplyr::coalesce(prop_area, 0))
  } else {
    final_res <- final_res %>%
      dplyr::mutate(
        dplyr::across(c(...), ~ dplyr::coalesce(.x, 0))
      )
  }

  return(final_res)
}

#' Aggregate tree data to plot or subplot levels
#'
#' @param object A EvalHandler object.
#' @param ... Variables to aggregate (tidy-select supported)
#' @param level The level to aggregate to. Can be "plot" or "subplot".
#' @keywords internal
.make_tree_aggregates <- function(object, ..., level = "plot") {
  # Join core tables
  # PLOT -> COND -> TREE
  
  # Start with PLOT
  res <- object@plot %>%
    dplyr::inner_join(object@cond, by = c("CN" = "PLT_CN"), suffix = c("", ".cond")) %>%
    dplyr::inner_join(object@tree, by = c("CN" = "PLT_CN", "CONDID" = "CONDID"), suffix = c("", ".tree"))
  
  # Apply standard filtering
  res <- res %>%
    dplyr::filter(
      !is.na(TPA_UNADJ)
    )
  
  # Apply pending mutations from the handler object
  # These are user-defined expressions queued via mutate_tree()
  if (length(object@tree_mutations) > 0) {
    res <- res %>% dplyr::mutate(!!!object@tree_mutations)
  }
  
  # Capture grouping variables
  # Standard PLOT keys
  plot_keys <- c("CN", "STATECD", "INVYR", "PLOT", "COUNTYCD")
  
  # Determine target variables for aggregation
  target_vars <- dplyr::quos(...)

  # Aggregate matching records
  # We group by PLOT keys + any user defined groups
  grouping_cols <- plot_keys
  if (length(object@tree_domains) > 0) {
     # Extract variable names from quosures for simpler checking 
     # (This is a simplification, assumes simple variable names or needs inspection)
     # For now, we rely on the fact that group_by adds them to the query
     res <- res %>% dplyr::group_by(!!!object@tree_domains)
  }
  
  # We also need to group by plot keys to ensure plot-level summary
  res <- res %>% 
    dplyr::group_by(dplyr::across(dplyr::all_of(plot_keys)), .add = TRUE)

  # Perform the aggregation (summing)
  # This results in a dataset with PLOT keys + Grouping Vars + Aggregated Metrics
  # But MISSING combinations that don't exist in TREE
  aggregated <- res %>%
    dplyr::summarise(
      dplyr::across(c(...), ~ sum(TPA_UNADJ * .x, na.rm = TRUE))
    ) %>%
    dplyr::ungroup()

  # ----------------------------------------------------------------
  # SCAFFOLD CONSTRUCTION
  # ----------------------------------------------------------------
  
  # 1. Get all plots (Full Plot List)
  all_plots <- object@plot %>%
    dplyr::select(dplyr::all_of(plot_keys))
  
  # 2. Identify Domain Variables (Grouping variables that are NOT plot keys)
  # We need to extract the unique combinations of these from the aggregated data
  # because we can't easily know the full domain of a 'cut' or 'factor' without evaluating it,
  # but we can assume the aggregated result contains the relevant observed domains.
  # (Or should we compute it from the mutated tree table? 
  #  Aggregated is safer as it's already computed).
  
  # Helper to identify column names that are in aggregated but NOT in plot_keys.
  # We use the grouping variables defined on the 'res' object before aggregation.
  all_groups <- dplyr::group_vars(res)
  
  # Domain vars are everything else (grouping vars that are not plot keys)
  domain_vars <- setdiff(all_groups, plot_keys)
  
  # Construct the scaffold
  if (length(domain_vars) > 0) {
    # Extract distinct combinations of domain variables from the aggregated data
    # NOTE: This assumes that "All possible domains" are present *somewhere* in the data across all plots.
    # If a domain level exists in theory but NO plot has it, it won't be here.
    # This is usually acceptable for "observed" domains.
    observed_domains <- aggregated %>%
      dplyr::select(dplyr::all_of(domain_vars)) %>%
      dplyr::distinct()
    
    # Cross join: All Plots x Observed Domains
    scaffold <- all_plots %>%
      dplyr::cross_join(observed_domains, copy = TRUE)
      
    # Join key is Plot Keys + Domain Vars
    join_by <- c(plot_keys, domain_vars)
    
  } else {
    # No extra grouping variables, just plots
    scaffold <- all_plots
    join_by <- plot_keys
  }

  # ----------------------------------------------------------------
  # FINAL MERGE
  # ----------------------------------------------------------------
  
  # Left join aggregated data onto the scaffold
  final_res <- scaffold %>%
    dplyr::left_join(aggregated, by = join_by) %>%
    dplyr::mutate(
      dplyr::across(c(...), ~ dplyr::coalesce(.x, 0))
    )

  return(final_res)
}

#' Summarize Trees to Plot Level
#'
#' @param object A EvalHandler object.
#' @param ... Variables to aggregate (tidy-select supported)
#' @return A lazy query with plot-level summaries.
#' @export
setMethod("summarize_tree", "EvalHandler", function(object, ...) {
  .make_tree_aggregates(object, ...)
})

#' Summarize Conditions to Plot Level
#'
#' @param object A EvalHandler object.
#' @param ... Variables to aggregate (tidy-select supported)
#' @return A lazy query with plot-level summaries.
#' @export
setMethod("summarize_cond", "EvalHandler", function(object, ..., prop_basis = "MACR") {
  .make_cond_aggregates(object, ..., prop_basis = prop_basis)
})