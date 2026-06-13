setClass("PostStratifiedEstimator",
  contains = "Estimator",
  slots = list(
    strata_weights = "ANY"
  )
)

#' PostStratifiedEstimator
#' 
#' Create an object that can be used to make post-stratified estimates. The
#' estimator is initialized with an `EvalHandler` that defines the evaluation.
#' 
#' @param handler A EvalHandler object.
#' @export
PostStratifiedEstimator <- function(handler) {
  new("PostStratifiedEstimator",
    handler = handler,
    strata_weights = get_strata_weights(handler)
  )
}


#' Show Method for PostStratifiedEstimator
#'
#' @param object A PostStratifiedEstimator object.
#' @export
setMethod("show", "PostStratifiedEstimator", function(object) {
  cat("PostStratifiedEstimator\n")
  cat("-----------------------\n")

  s <- summary(object@handler)

  cat("EVALID:         ", object@handler@evalid, "\n")

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

  n_estn_units <- object@handler@tables$pop_estn_unit %>%
    dplyr::tally() %>%
    dplyr::collect() %>%
    dplyr::pull(n)

  n_strata <- object@handler@tables$pop_stratum %>%
    dplyr::tally() %>%
    dplyr::collect() %>%
    dplyr::pull(n)

  cat("Estn Units:     ", n_estn_units, "\n")
  cat("Strata:         ", n_strata, "\n")
  cat("Plots:          ", s$n_plots, "\n")
})


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
#' @return A dataframe with estimates.
#' @export
setMethod("estimate", "PostStratifiedEstimator", function(object, ..., output = "mean", margins = FALSE) {
  args <- list(...)
  if (length(args) != 1) {
    stop("Must provide exactly one scoped target helper, such as `tree(VOLCFGRS)` or `cond()`.")
  }
  output <- match.arg(output, c("mean", "total"))

  parsed <- .parse_target_spec(args[[1]], "estimate")
  slot_name <- parsed$slot
  targets <- parsed$targets

  if (slot_name == "cond") {
    if (length(targets) > 0 && !all(targets == "1")) {
      stop("Only `estimate(cond())` or `estimate(cond(1))` is currently supported for condition estimates.")
    }
    return(.estimate_cond_internal(object, output = output, margins = margins))
  } else if (slot_name == "tree") {
    return(.estimate_tree_internal(object, targets, output = output, margins = margins))
  } else {
    stop("Unsupported slot: ", slot_name)
  }
})

# Internal helper: return all subsets of a list (including the empty set).
# Used to generate every combination of domain variable subsets for marginals.
.all_subsets <- function(lst) {
  n <- length(lst)
  lapply(0:(2^n - 1), function(mask) {
    keep <- as.logical(intToBits(mask)[seq_len(n)])
    lst[keep]
  })
}

# Run the full post-stratification pipeline for the given handler.
# The handler's tree_domains and cond_domains determine grouping.
.run_tree_estimation <- function(handler, targets, output = "mean") {
  syms <- rlang::syms(targets)
  plot_data <- .make_tree_aggregates(handler, !!!syms, adjusted = TRUE, sparse = TRUE)
  strata_data <- .ps_join_strata(plot_data, handler)
  strata_stats <- .ps_strata_stats(strata_data, targets)
  eu_stats <- .ps_eu_stats(strata_stats, targets)
  .ps_pop_stats(eu_stats, handler, targets, output = output)
}

.run_cond_estimation <- function(handler, output = "mean") {
  plot_data <- .make_cond_aggregates(handler, adjusted = TRUE, sparse = TRUE)
  strata_data <- .ps_join_strata(plot_data, handler)
  strata_stats <- .ps_strata_stats(strata_data, "prop")
  eu_stats <- .ps_eu_stats(strata_stats, "prop")
  .ps_pop_stats(eu_stats, handler, "prop", output = output)
}

# Internal helper for condition estimation
.estimate_cond_internal <- function(object, output = "mean", margins = FALSE) {
  if (!margins) {
    return(.run_cond_estimation(object@handler, output = output))
  }

  n_full <- length(object@handler@cond_domains)
  # Iterate over every subset of the active cond domains (includes grand total).
  cond_subsets <- .all_subsets(object@handler@cond_domains)
  results <- lapply(cond_subsets, function(dom) {
    h <- object@handler
    h@cond_domains <- dom
    res <- .run_cond_estimation(h, output = output)
    res$is_marginal <- length(dom) < n_full
    res
  })
  dplyr::bind_rows(results)
}

# Internal helper for tree estimation
.estimate_tree_internal <- function(object, targets, output = "mean", margins = FALSE) {
  if (!margins) {
    return(.run_tree_estimation(object@handler, targets, output = output))
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
      res <- .run_tree_estimation(h, targets, output = output)
      res$is_marginal <- !(length(t) == n_full_tree && length(c) == n_full_cond)
      results[[length(results) + 1]] <- res
    }
  }
  dplyr::bind_rows(results)
}
