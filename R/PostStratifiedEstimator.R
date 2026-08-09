setClass(
  "PostStratifiedEstimator",
  contains = "Estimator",
  slots = list(
    strata_weights = "ANY",
    var_est = "VarianceEstimator"
  )
)

#' PostStratifiedEstimator
#'
#' Create an object that can be used to make post-stratified estimates. The
#' estimator can be initialized with an `EvalHandler` that defines the
#' evaluation, or used as an unbound specification via `pe_post_strat()`.
#'
#' @param handler An optional EvalHandler object.
#' @param var_est A variance-estimator specification, or `"auto"` for the
#'   default non-ratio post-stratified variance estimator.
#' @export
PostStratifiedEstimator <- function(handler = NULL, var_est = "auto") {
  var_est <- .resolve_post_strat_var_est(var_est = var_est, context = "non_ratio")

  new(
    "PostStratifiedEstimator",
    handler = handler,
    strata_weights = if (is.null(handler)) NULL else get_strata_weights(handler),
    var_est = var_est
  )
}

#' Post-stratified Point Estimator
#'
#' This function implements the standard FIA post-stratified point estimator.
#' Astute statistical users will need to forgive a small abuse of nomenclature
#' here. Indeed, this estimator is a weighted sum of post-stratified estimators
#' across estimation units within an evaluation. We will prefer this simpler
#' term at the risk of some confusion, as it is the standard term used in FIA
#' documentation. Because the estimator is standard it requires the presence
#' of `pop_stratum` and `pop_estn_unit` tables in the evaluation, which
#' encode the post-strata weights among other details.
#' With the default `var_est = "auto"`, the standard
#' [ve_post_strat()][ve_post_strat] variance estimator is selected
#' automatically. This is equivalent to supplying `ve_post_strat()` explicitly
#' and provides a convenient default for the standard non-ratio estimator.
#'
#' Post-stratified point estimation, in the FIA context, computes estimates for
#' each estimation unit using a post-stratified estimator, then sums across the
#' set of estimation units (assuming independence) to produce a single estimate
#' for the evaluation. For estimation unit \eqn{g}{g} we obtain
#' \deqn{\hat{Y}_g = \sum_h W_{gh} \hat{Y}_{gh}}
#' where \eqn{h}{h} indexes a post-stratum within estimation unit $\eqn{g}{g}$
#' that unit, \eqn{\hat{Y}_{gh}}{Yhat_gh} is the stratum-specific estimate, and
#' \eqn{W_{gh}}{W_gh} is its post-stratum weight. Then, the overall estimate is
#' \deqn{\hat{Y} = \sum_g K_g \hat{Y}_g}
#' where \eqn{K_g}{K_g} is the proportion of the total evaluation area in
#' estimation unit \eqn{g}{g}. The estimator can return either the weighted
#' mean or the corresponding total through the `output` argument to
#' [estimate()][estimate], wherein each estimation unit is weighted by its size
#' in acres, \eqn{A_g}{A_g}.
#'
#' @param var_est A variance-estimator specification, or `"auto"`.
#' @return A `PostStratifiedEstimator` specification.
#' @export
pe_post_strat <- function(var_est = "auto") {
  PostStratifiedEstimator(
    var_est =
      var_est
  )
}

#' Post-stratified Ratio Point Estimator
#'
#' This function implements the FIA post-stratified ratio point estimator, a
#' technique commonly used to estimate areal densities subset to some land
#' basis of interest, like forested or timberland area. Much like the
#' [pe_post_strat()][pe_post_strat] estimator, this estimator
#' computes post-stratified ratios over a set of estimation units. The
#' estimator requires the presence of `pop_stratum` and `pop_estn_unit` tables
#' in the handler, typically present when using [eval_handler()][eval_handler].
#'
#' This estimator is composed of a ratio of two [pe_post_strat()][pe_post_strat]
#' estimators, one for the numerator and one for the denominator, yielding a
#' ratio estimate for the evaluation:
#' \deqn{\hat{R} = \frac{\sum_g K_g \hat{Y}_g}{\sum_g K_g \hat{X}_g}}
#' where \eqn{K_g} is the estimation unit weight and \eqn{\hat{Y}_g} and
#' \eqn{\hat{X}_g} are two estimators of the same form, documented further in
#' [pe_post_strat()][pe_post_strat].
#' With the default `var_est = "auto"`, the standard
#' [ve_post_strat_ratio()][ve_post_strat_ratio] variance estimator is selected
#' automatically. This is equivalent to supplying `ve_post_strat_ratio()`
#' explicitly and provides a convenient default for the standard ratio
#' estimator.
#'
#' @param var_est A variance-estimator specification, or `"auto"`.
#' @return A `PostStratifiedRatioEstimator` specification.
#' @export
pe_post_strat_ratio <- function(var_est = "auto") {
  PostStratifiedRatioEstimator(var_est = var_est)
}


#' Estimate Population Parameters
#'
#' @param object A PostStratifiedEstimator object.
#' @param ... Exactly one scope specifying the estimation target
#'   (e.g., `tree(VOLCFGRS)` or `cond()`).
#' @param output Output scale, either "mean" (default) or "total".
#' @param margins Logical. If `TRUE`, returns the full cross-domain estimates
#'   plus all marginal estimates produced by re-running the pipeline for every
#'   strict subset of the active domain variables (including the grand total
#'   with no domains). Dropped domain columns appear as `NA`. Defaults to
#'   `FALSE`.
#' @param estimator Unused for a bound estimator object.
#' @param var_est A variance-estimator specification, or `"auto"` (default) to
#'   use estimator-specific defaults.
#' @return A dataframe with estimates.
#' @export
setMethod(
  "estimate",
  "PostStratifiedEstimator",
  function(
    object,
    ...,
    output = "mean",
    margins = FALSE,
    estimator = pe_post_strat(),
    var_est = "auto"
  ) {
    if (is.null(object@handler)) {
      stop(
        "This estimator is not bound to a handler. Pass it as `estimate(handler, target, estimator = this_estimator)`.",
        call. = FALSE
      )
    }

    args <- list(...)
    if (length(args) == 0) {
      stop(
        "Must provide at least one scope, such as `tree(VOLCFGRS)`, `cond()`, or `dwm(dwm_cwd(CARBON))`."
      )
    }
    output <- match.arg(output, c("mean", "total"))

    if (any(vapply(args, inherits, logical(1), "fiaplyr_ratio_intent"))) {
      stop(
        "A non-ratio estimator requires a ratio point estimator for `ratio(...)` targets. Use `estimator = pe_post_strat_ratio(...)` or `estimator = \"auto\"`.",
        call. = FALSE
      )
    }

    parsed_list <- lapply(args, .parse_target_spec, caller = "estimate")

    results <- lapply(parsed_list, function(parsed) {
      .estimate_parsed_target(object, parsed, output = output, margins = margins)
    })

    .lazy_bind_rows(results)
  }
)

# Internal helper: run one parsed target through the appropriate per-scope
# estimation pipeline. Multiple targets are handled by the caller, which stacks
# the results row-wise with `.lazy_bind_rows()`.
.estimate_parsed_target <- function(object, parsed, output = "mean", margins = FALSE) {
  slot_name <- parsed$slot
  targets <- parsed$targets
  target_names <- parsed$target_names

  if (slot_name == "cond") {
    if (length(targets) > 0 && !all(targets == "1")) {
      stop(
        "Only `estimate(cond())` or `estimate(cond(1))` is currently supported for condition estimates."
      )
    }
    return(.estimate_cond_internal(
      object,
      targets = targets,
      target_names = target_names,
      output = output,
      margins = margins
    ))
  } else if (slot_name == "tree") {
    if (inherits(object@handler@spec, "DWMAnalysis")) {
      stop(
        "`dwm_analysis()` does not support `estimate(tree(...))` targets; use DWM component helpers wrapped in `dwm()`.",
        call. = FALSE
      )
    }
    return(.estimate_tree_internal(
      object,
      targets,
      target_names = target_names,
      target_quos = parsed$quosures,
      output = output,
      margins = margins
    ))
  } else if (slot_name == "tree_history") {
    if (!inherits(object@handler@spec, "GRMAnalysis")) {
      stop("`estimate(tree_history(...))` requires a GRMAnalysis handler.")
    }
    return(.estimate_tree_history_internal(
      object,
      targets,
      target_names = target_names,
      target_quos = parsed$quosures,
      output = output,
      margins = margins
    ))
  } else if (slot_name == "dwm") {
    if (!inherits(object@handler@spec, "DWMAnalysis")) {
      stop("DWM estimation requires a `dwm_analysis()` handler.", call. = FALSE)
    }
    if (is.null(parsed$dwm_targets)) {
      stop(
        "DWM estimation requires a component helper wrapped in `dwm()`, such as `dwm(dwm_cwd(CARBON))`.",
        call. = FALSE
      )
    }
    return(.estimate_dwm_internal(
      object,
      parsed$dwm_targets,
      output = output,
      margins = margins
    ))
  } else {
    stop("Unsupported slot: ", slot_name)
  }
}

setMethod(
  ".estimate_composed",
  signature(
    point_estimator = "PostStratifiedEstimator",
    variance_estimator = "PostStratifiedVarianceEstimator"
  ),
  function(
    point_estimator,
    variance_estimator,
    handler,
    target,
    ...,
    output = "mean",
    margins = FALSE
  ) {
    extra_args <- list(...)
    all_targets <- c(list(target), extra_args)

    if (any(vapply(all_targets, inherits, logical(1), "fiaplyr_ratio_intent"))) {
      stop(
        "A non-ratio estimator requires a ratio point estimator for `ratio(...)` targets. Use `estimator = pe_post_strat_ratio(...)` or `estimator = \"auto\"`.",
        call. = FALSE
      )
    }

    bound_estimator <- PostStratifiedEstimator(
      handler,
      var_est = variance_estimator
    )
    do.call(
      estimate,
      c(
        list(object = bound_estimator),
        list(target),
        extra_args,
        list(output = output, margins = margins)
      )
    )
  }
)

# Internal helper: return all subsets of a list (including the empty set).
# Used to generate every combination of domain variable subsets for marginals.
.all_subsets <- function(lst) {
  n <- length(lst)
  lapply(0:(2^n - 1), function(mask) {
    keep <- as.logical(intToBits(mask)[seq_len(n)])
    lst[keep]
  })
}

# Stack lazy tbls row-wise while preserving laziness. Marginal subsets drop
# domain columns, so inputs can have different column sets. Align every input
# to the union of columns (filling absent ones with NA) so a positional
# union_all produces the same NA-filled layout that bind_rows would locally.
.lazy_bind_rows <- function(results) {
  results <- Filter(Negate(is.null), results)
  if (length(results) == 0) {
    return(NULL)
  }
  if (length(results) == 1) {
    return(results[[1]])
  }

  all_cols <- Reduce(union, lapply(results, colnames))
  aligned <- lapply(results, function(res) {
    missing <- setdiff(all_cols, colnames(res))
    if (length(missing) > 0) {
      na_cols <- stats::setNames(rep(list(NA), length(missing)), missing)
      res <- res %>% dplyr::mutate(!!!na_cols)
    }
    res %>% dplyr::select(dplyr::all_of(all_cols))
  })

  Reduce(dplyr::union_all, aligned)
}

.resolve_estimation_targets <- function(
  targets,
  target_names = NULL,
  target_quos = NULL
) {
  if (!is.null(target_quos)) {
    return(.resolve_tree_target_names(target_quos))
  }

  resolved_targets <- targets
  if (!is.null(target_names) && length(target_names) == length(targets)) {
    resolved_targets <- ifelse(nzchar(target_names), target_names, targets)
  }

  resolved_targets
}

# Run the full post-stratification pipeline for the given handler.
# The handler's pipeline domains determine grouping.
.run_tree_estimation <- function(
  handler,
  targets,
  target_names = NULL,
  target_quos = NULL,
  output = "mean"
) {
  estimation_target_quos <- target_quos
  if (is.null(estimation_target_quos)) {
    estimation_target_quos <- rlang::syms(targets)
    if (!is.null(target_names)) {
      names(estimation_target_quos) <- target_names
    }
  } else if (is.list(estimation_target_quos) && length(estimation_target_quos) == 0) {
    # An empty tree helper requests the implicit tree-count target.
    estimation_target_quos <- list(rlang::quo(1))
    names(estimation_target_quos) <- "tree_count"
  }

  resolved_targets <- .resolve_estimation_targets(
    targets,
    target_names,
    target_quos = estimation_target_quos
  )

  plot_data <- .make_tree_aggregates(
    handler,
    !!!estimation_target_quos,
    adjusted = TRUE,
    sparse = TRUE
  )
  strata_data <- .ps_join_strata(plot_data, handler)
  strata_stats <- .ps_strata_stats(strata_data, resolved_targets)
  eu_stats <- .ps_eu_stats(strata_stats, resolved_targets)
  .ps_pop_stats(eu_stats, handler, resolved_targets, output)
}

.run_tree_history_estimation <- function(
  handler,
  targets,
  target_names = NULL,
  target_quos = NULL,
  output = "mean"
) {
  agg_targets <- target_quos
  if (is.null(agg_targets)) {
    agg_targets <- rlang::syms(targets)
    if (!is.null(target_names)) {
      names(agg_targets) <- target_names
    }
  }

  resolved_targets <- .resolve_estimation_targets(
    targets,
    target_names,
    target_quos
  )

  plot_data <- .make_tree_history_aggregates(
    handler,
    !!!agg_targets,
    adjusted = TRUE,
    sparse = TRUE
  )
  strata_data <- .ps_join_strata(plot_data, handler)
  strata_stats <- .ps_strata_stats(strata_data, resolved_targets)
  eu_stats <- .ps_eu_stats(strata_stats, resolved_targets)
  .ps_pop_stats(eu_stats, handler, resolved_targets, output)
}

.run_cond_estimation <- function(
  handler,
  target_names = NULL,
  output = "mean"
) {
  plot_data <- .make_cond_aggregates(handler, adjusted = TRUE, sparse = TRUE)
  if (
    !is.null(target_names) &&
      length(target_names) == 1 &&
      nzchar(target_names[[1]])
  ) {
    plot_data <- plot_data %>% dplyr::rename(!!target_names[[1]] := prop)
    cond_target <- target_names[[1]]
  } else {
    cond_target <- "prop"
  }

  strata_data <- .ps_join_strata(plot_data, handler)
  strata_stats <- .ps_strata_stats(strata_data, cond_target)
  eu_stats <- .ps_eu_stats(strata_stats, cond_target)
  .ps_pop_stats(eu_stats, handler, cond_target, output)
}

.run_dwm_estimation <- function(handler, targets, output = "mean") {
  resolved_targets <- vapply(targets, function(target) target$name, character(1))
  plot_data <- .make_dwm_aggregates(
    handler,
    targets,
    adjusted = TRUE,
    sparse = TRUE
  )
  strata_data <- .ps_join_strata(plot_data, handler)
  strata_stats <- .ps_strata_stats(strata_data, resolved_targets)
  eu_stats <- .ps_eu_stats(strata_stats, resolved_targets)
  .ps_pop_stats(eu_stats, handler, resolved_targets, output)
}

# Internal helper for DWM estimation
.estimate_dwm_internal <- function(
  object,
  targets,
  output = "mean",
  margins = FALSE
) {
  if (!margins) {
    return(.run_dwm_estimation(object@handler, targets, output = output))
  }

  scopes <- c("plot", "cond", "dwm")
  full_counts <- vapply(
    scopes,
    function(scope) length(.pipeline_domains(object@handler, scope)),
    integer(1)
  )
  subsets <- lapply(
    scopes,
    function(scope) .all_subsets(.pipeline_domains(object@handler, scope))
  )

  results <- list()
  for (p in subsets[[1]]) {
    for (c in subsets[[2]]) {
      for (d in subsets[[3]]) {
        handler <- object@handler
        handler <- .pipeline_update(handler, "plot", "domain", p, "replace")
        handler <- .pipeline_update(handler, "cond", "domain", c, "replace")
        handler <- .pipeline_update(handler, "dwm", "domain", d, "replace")
        is_marginal <- !identical(
          c(length(p), length(c), length(d)),
          unname(full_counts)
        )
        results[[length(results) + 1]] <- .run_dwm_estimation(
          handler,
          targets,
          output = output
        ) %>%
          dplyr::mutate(is_marginal = !!is_marginal)
      }
    }
  }

  .lazy_bind_rows(results)
}

# Internal helper for condition estimation
.estimate_cond_internal <- function(
  object,
  targets = character(0),
  target_names = character(0),
  output = "mean",
  margins = FALSE
) {
  cond_target_names <- target_names
  if (length(targets) == 0) {
    cond_target_names <- character(0)
  }

  if (!margins) {
    return(.run_cond_estimation(
      object@handler,
      target_names = cond_target_names,
      output = output
    ))
  }

  n_full <- length(.pipeline_domains(object@handler, "cond"))
  # Iterate over every subset of the active cond domains (includes grand total).
  cond_subsets <- .all_subsets(.pipeline_domains(object@handler, "cond"))
  results <- lapply(cond_subsets, function(dom) {
    h <- object@handler
    h <- .pipeline_update(h, "cond", "domain", dom, "replace")
    is_marg <- length(dom) < n_full
    .run_cond_estimation(
      h,
      target_names = cond_target_names,
      output = output
    ) %>%
      dplyr::mutate(is_marginal = !!is_marg)
  })
  .lazy_bind_rows(results)
}

# Internal helper for tree estimation
.estimate_tree_internal <- function(
  object,
  targets,
  target_names = NULL,
  target_quos = NULL,
  output = "mean",
  margins = FALSE
) {
  if (!margins) {
    return(.run_tree_estimation(
      object@handler,
      targets,
      target_names = target_names,
      target_quos = target_quos,
      output = output
    ))
  }

  n_full_tree <- length(.pipeline_domains(object@handler, "tree"))
  n_full_cond <- length(.pipeline_domains(object@handler, "cond"))

  # Iterate over every (tree_domain_subset, cond_domain_subset) combination.
  # bind_rows fills dropped domain columns with NA, which signals "all values".
  tree_subsets <- .all_subsets(.pipeline_domains(object@handler, "tree"))
  cond_subsets <- .all_subsets(.pipeline_domains(object@handler, "cond"))

  results <- list()
  for (t in tree_subsets) {
    for (c in cond_subsets) {
      h <- object@handler
      h <- .pipeline_update(h, "tree", "domain", t, "replace")
      h <- .pipeline_update(h, "cond", "domain", c, "replace")
      is_marg <- !(length(t) == n_full_tree && length(c) == n_full_cond)
      res <- .run_tree_estimation(
        h,
        targets,
        target_names = target_names,
        target_quos = target_quos,
        output = output
      ) %>%
        dplyr::mutate(is_marginal = !!is_marg)
      results[[length(results) + 1]] <- res
    }
  }
  .lazy_bind_rows(results)
}

# Internal helper for tree history estimation
.estimate_tree_history_internal <- function(
  object,
  targets,
  target_names = NULL,
  target_quos = NULL,
  output = "mean",
  margins = FALSE
) {
  if (!margins) {
    return(.run_tree_history_estimation(
      object@handler,
      targets,
      target_names = target_names,
      target_quos = target_quos,
      output = output
    ))
  }

  n_full_cond <- length(.pipeline_domains(object@handler, "cond"))
  n_full_tree_history <- length(.pipeline_domains(object@handler, "tree_history"))

  cond_subsets <- .all_subsets(.pipeline_domains(object@handler, "cond"))
  tree_history_subsets <- .all_subsets(.pipeline_domains(object@handler, "tree_history"))

  results <- list()
  for (c in cond_subsets) {
    for (th in tree_history_subsets) {
      h <- object@handler
      h <- .pipeline_update(h, "cond", "domain", c, "replace")
      h <- .pipeline_update(h, "tree_history", "domain", th, "replace")
      is_marg <- !(length(c) == n_full_cond &&
        length(th) == n_full_tree_history)
      res <- .run_tree_history_estimation(
        h,
        targets,
        target_names = target_names,
        target_quos = target_quos,
        output = output
      ) %>%
        dplyr::mutate(is_marginal = !!is_marg)
      results[[length(results) + 1]] <- res
    }
  }

  .lazy_bind_rows(results)
}
