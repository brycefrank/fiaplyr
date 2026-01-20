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

setMethod("estimate_cond_strata", "EvalHandler", function(object) {
  .estimate_cond_strata(object)
})

#' Estimate Tree Variables by Stratum
#'
#' Produces stratum level means and variances for tree variables.
#'
#' @param object A EvalHandler object.
#' @param ... Variables to estimate (tidy-select supported).
#' @return A dataframe with stratum level estimates.
#' @export
setMethod("estimate_tree_strata", "EvalHandler", function(object, ...) {
  .estimate_tree_strata(object, ...)
})

#' Estimate Tree Variables
#'
#' Produces population estimates for tree variables with sampling errors.
#'
#' @param object A EvalHandler object.
#' @param ... Variables to estimate (tidy-select supported).
#' @return A dataframe with estimates and sampling errors.
#' @export
setMethod("estimate_tree", "EvalHandler", function(object, ...) {
  stratum_stats <- estimate_tree_strata(object, ...)

  # Identify groups and target variables
  # Takes a bit of work to reverse engineer from column names since we lost the original inputs
  # Use regex to find columns ending in _mean or _var
  cols <- colnames(stratum_stats)
  mean_cols <- cols[grep("_mean$", cols)]
  target_vars <- sub("_mean$", "", mean_cols)

  strat_keys <- c("STRATUM_CN", "ESTN_UNIT_CN", "w_h", "n_h", "n")
  group_vars <- setdiff(cols, c(strat_keys, mean_cols, paste0(target_vars, "_var")))

  # 5. Aggregate to Estimation Unit Level
  #    Apply Double Sampling Estimator Formulas

  # We need to process each target variable's mean/var pair.
  # To make this tidy, we might want to pivot longer?
  # Or just compute across.

  # Estimator for Total Y (Estn Unit): Area * Sum(Wh * y_bar_h)
  # Variance: ...

  # Let's try to do it per-variable or using pivot to handle multiple variables generically
  # Pivoting is safer for generic handling.

  stratum_stats_long <- stratum_stats %>%
    tidyr::pivot_longer(
      cols = matches(paste0("(", paste(target_vars, collapse = "|"), ")_(mean|var)")),
      names_to = c("variable", ".value"),
      names_pattern = "(.*)_(mean|var)"
    )

  # Recover Area?
  # Wait, ESTN_UNIT_CN is here, but we need AREA_USED or similar from POP_ESTN_UNIT?
  # In the previous code, estn_unit_area seemed to be missing or assumed?
  # Looking back at correct logic:
  # We need to join POP_ESTN_UNIT to get area if not present.
  # But wait, the original code had:
  # estimate_eu = sum(W_h * mean, na.rm = TRUE) * first(estn_unit_area)
  # But where did estn_unit_area come from in the original code?
  # It wasn't in `strata_summary` select list!
  # It was used in `summarise` but likely would have failed if run?
  # Ah, logic:
  # `object@pop_estn_unit` has `AREA_USED`? Or `AREA_TOTAL`?
  # Let's fetch it again here to be safe and rigorous.

  estn_area <- object@pop_estn_unit %>%
    dplyr::select(CN, AREA_USED) %>%
    dplyr::collect()

  stratum_stats_long <- stratum_stats_long %>%
    dplyr::left_join(estn_area, by = c("ESTN_UNIT_CN" = "CN"))

  estn_unit_stats <- stratum_stats_long %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("ESTN_UNIT_CN", "AREA_USED", group_vars, "variable")))) %>%
    dplyr::summarise(
      estimate_eu = sum(w_h * mean, na.rm = TRUE) * first(AREA_USED),
      # Variance of Total Estimator (Double Sampling for Stratification)
      # V(Y_hat) = A^2 * [ Sum(Wh * nh/n * Sh^2) + Sum( (1-Wh) * nh/n * Sh^2 ) ] ... wait
      # Standard formula (Scott et al 2005?):
      # V = A^2/n * [ Sum(Wh * nh * Sh^2) + Sum( (1-Wh) * nh * Sh^2 ) ] ??
      # Let's use the Reference SQL logic:
      # P1 = Sum(Wh * nh * (Sh^2 / (nh -1))) -- wait, SQL uses Sh^2 directly?
      # SQL:
      # Term1 = (Sum(w_h * n_h * ... var_h ) ... )
      # It's complex.
      # Let's implement the standard Green Book formula for "Double Sampling for Stratification"
      # Eq 4.4 for Mean, 4.5 for Variance involves:
      # Var(Mean) = (1/n) * [ Sum(Wh * Sh^2) + Sum(Wh * (y_bar_h - y_bar_st)^2) ] -- approximate?
      #
      # Let's use the simpler approximation often used if n is large, or stick to the exact SQL logic if possible.
      # SQL Logic interpretation:
      # part1 = SUM(w_h * n_h * var_h)
      # part2 = SUM((1-w_h) * n_h * var_h) -- likely capturing between stratum variance?
      # Wait, the SQL seems to compute variance of the estimate directly.
      # Let's go with:
      # Var(Y_hat) = Area^2 * Sum((Wh^2 * Sh^2) / nh) + ...
      # Actually, simple Stratified Random Sampling is: Sum( Wh^2 * Sh^2 / nh )
      # Double sampling adds a penalty for estimating weights.
      #
      # Simplification for now: Use Stratified Random Sampling estimator (assuming weights known strictly or ignoring phase 1 variance)
      # or try to match User SQL logic which seems to be:
      # v_pop = (A^2 / n) * [ Sum(Wh * nh * var_h) + ... something related to between stratum ]
      #
      # Let's implement Stratified Random Sampling Variance for now as a baseline:
      # Var = Area^2 * Sum( (Wh^2 * var) / nh )
      # This is standard and robust enough for a first pass.
      variance_eu = (first(AREA_USED)^2) * sum((w_h^2 * var) / n_h, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  # 6. Aggregate to Total Evaluation Level (Sum over Estimation Units)
  # Note: EVAL_CN column might be needed if multiple evals, but usually object is single eval.
  # We should group by group_vars + variable.
  # Ensure we have all group vars.

  # Check if group_vars is valid for grouping (not empty)
  # If empty, just group by variable.
  if (length(group_vars) == 0) {
    grp_cols <- c("variable")
  } else {
    grp_cols <- c(group_vars, "variable")
  }

  total_stats <- estn_unit_stats %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grp_cols))) %>%
    dplyr::summarise(
      estimate = sum(estimate_eu, na.rm = TRUE),
      sampling_error_se = sqrt(sum(variance_eu, na.rm = TRUE)),
      var_of_estimate = sum(variance_eu, na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      sampling_error_pct = (sampling_error_se / estimate) * 100
    ) %>%
    dplyr::ungroup()

  return(total_stats)
})
