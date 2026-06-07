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
#' @param ... One or more formulas specifying estimation targets
#'   (e.g., tree ~ VOLCFGRS).
#' @param output Output scale, either "mean" (default) or "total".
#' @return A dataframe with estimates.
#' @export
setMethod("estimate", "PostStratifiedEstimator", function(object, ..., output = "mean") {
  formulas <- list(...)
  if (length(formulas) == 0) stop("Must provide at least one formula.")
  if (!all(vapply(formulas, inherits, logical(1), "formula"))) {
    stop("All unnamed arguments must be formulas.")
  }
  output <- match.arg(output, c("mean", "total"))

  results <- lapply(formulas, function(formula) {
    parsed <- parse_formula(formula)
    slot_name <- parsed$slot
    targets <- parsed$targets

    if (slot_name == "cond") {
      if (!all(targets == "1")) {
        stop("Only 'cond ~ 1' is currently supported for condition estimates.")
      }
      return(.estimate_cond_internal(object, output = output))
    } else if (slot_name == "tree") {
      return(.estimate_tree_internal(object, targets, output = output))
    } else {
      stop("Unsupported slot: ", slot_name)
    }
  })

  if (length(results) == 1) {
    return(results[[1]])
  }

  dplyr::bind_rows(results)
})

# Internal helper for condition estimation
.estimate_cond_internal <- function(object, output = "mean") {
  plot_data <- .make_cond_aggregates(object@handler, adjusted = TRUE, sparse = TRUE)
  targets <- "prop"

  strata_data <- .ps_join_strata(plot_data, object@handler)
  strata_stats <- .ps_strata_stats(strata_data, targets)
  eu_stats <- .ps_eu_stats(strata_stats, targets)
  .ps_pop_stats(eu_stats, object@handler, targets, output = output)
}

# Internal helper for tree estimation
.estimate_tree_internal <- function(object, targets, output = "mean") {
  syms <- rlang::syms(targets)
  plot_data <- .make_tree_aggregates(object@handler, !!!syms, adjusted = TRUE, sparse = TRUE)

  strata_data <- .ps_join_strata(plot_data, object@handler)
  strata_stats <- .ps_strata_stats(strata_data, targets)
  eu_stats <- .ps_eu_stats(strata_stats, targets)
  .ps_pop_stats(eu_stats, object@handler, targets, output = output)
}
