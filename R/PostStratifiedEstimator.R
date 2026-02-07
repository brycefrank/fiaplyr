setClass("PostStratifiedEstimator",
  contains = "Estimator",
  slots = list(
    strata_weights = "ANY"
  )
)

#' Constructor for PostStratifiedEstimator
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
#' @param ... A formula specifying the estimation target (e.g., tree ~ VOLCFGRS).
#' @return A dataframe with estimates.
#' @export
setMethod("estimate", "PostStratifiedEstimator", function(object, ...) {
  args <- list(...)
  if (length(args) == 0) stop("Must provide a formula.")
  formula <- args[[1]]

  parsed <- parse_formula(formula)
  slot_name <- parsed$slot
  targets <- parsed$targets

  if (slot_name == "cond") {
    if (!all(targets == "1")) {
      stop("Only 'cond ~ 1' is currently supported for condition estimates.")
    }
    return(.estimate_cond_internal(object))
  } else if (slot_name == "tree") {
    return(.estimate_tree_internal(object, targets))
  } else {
    stop("Unsupported slot: ", slot_name)
  }
})

# Internal helper for condition estimation
.estimate_cond_internal <- function(object) {
  plot_data <- .make_cond_aggregates(object@handler, adjusted = TRUE, sparse = TRUE)
  targets <- "prop"

  strata_data <- .ps_join_strata(plot_data, object@handler)
  strata_means <- .ps_strata_means(strata_data, targets)
  eu_data <- .ps_eu_estimates(strata_means, targets)
  .ps_pop_estimates(eu_data, object@handler, targets)
}

# Internal helper for tree estimation
.estimate_tree_internal <- function(object, targets) {
  syms <- rlang::syms(targets)
  plot_data <- .make_tree_aggregates(object@handler, !!!syms, adjusted = TRUE, sparse = TRUE)

  strata_data <- .ps_join_strata(plot_data, object@handler)
  strata_means <- .ps_strata_means(strata_data, targets)
  eu_data <- .ps_eu_estimates(strata_means, targets)
  .ps_pop_estimates(eu_data, object@handler, targets)
}
