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

#' Configure Post-Stratified Point Estimation
#'
#' Creates an estimator specification for use with
#' `estimate(handler, target, estimator = pe_post_strat())`.
#'
#' @param var_est A variance-estimator specification, or `"auto"`.
#' @return A `PostStratifiedEstimator` specification.
#' @export
pe_post_strat <- function(var_est = "auto") {
  PostStratifiedEstimator(var_est = var_est)
}

#' Configure Post-Stratified Ratio Point Estimation
#'
#' Creates a ratio-estimator specification for use with
#' `estimate(handler, ratio(...), estimator = pe_post_strat_ratio())`.
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
#' @param ... Exactly one scoped target helper specifying the estimation target
#'   (e.g., `tree(VOLCFGRS)` or `cond()`).
#' @param output Output scale, either "mean" (default) or "total".
#' @param margins Logical. If `TRUE`, returns the full cross-domain estimates
#'   plus all marginal estimates produced by re-running the pipeline for every
#'   strict subset of the active domain variables (including the grand total
#'   with no domains). Dropped domain columns appear as `NA`. Defaults to
#'   `FALSE`.
#' @param estimator Unused for a bound estimator object.
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
    if (length(args) != 1) {
      stop(
        "Must provide exactly one scoped target helper, such as `tree(VOLCFGRS)` or `cond()`."
      )
    }
    output <- match.arg(output, c("mean", "total"))

    parsed <- .parse_target_spec(args[[1]], "estimate")
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
    } else {
      stop("Unsupported slot: ", slot_name)
    }
  }
)

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

    if (inherits(target, "fiaplyr_ratio_intent")) {
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
# The handler's tree_domains and cond_domains determine grouping.
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

  n_full <- length(object@handler@cond_domains)
  # Iterate over every subset of the active cond domains (includes grand total).
  cond_subsets <- .all_subsets(object@handler@cond_domains)
  results <- lapply(cond_subsets, function(dom) {
    h <- object@handler
    h@cond_domains <- dom
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

  n_full_tree <- length(object@handler@tree_domains)
  n_full_cond <- length(object@handler@cond_domains)

  # Iterate over every (tree_domain_subset, cond_domain_subset) combination.
  # bind_rows fills dropped domain columns with NA, which signals "all values".
  tree_subsets <- .all_subsets(object@handler@tree_domains)
  cond_subsets <- .all_subsets(object@handler@cond_domains)

  results <- list()
  for (t in tree_subsets) {
    for (c in cond_subsets) {
      h <- object@handler
      h@tree_domains <- t
      h@cond_domains <- c
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

  n_full_cond <- length(object@handler@cond_domains)
  n_full_tree_history <- length(object@handler@tree_history_domains)

  cond_subsets <- .all_subsets(object@handler@cond_domains)
  tree_history_subsets <- .all_subsets(object@handler@tree_history_domains)

  results <- list()
  for (c in cond_subsets) {
    for (th in tree_history_subsets) {
      h <- object@handler
      h@cond_domains <- c
      h@tree_history_domains <- th
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
