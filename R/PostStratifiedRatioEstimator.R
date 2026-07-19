#' PostStratifiedRatioEstimator Class
#'
#' @slot handler An EvalHandler object.
#' @slot strata_weights A dataframe containing strata weights.
#' @slot var_est A variance-estimator specification.
#' @export
setClass(
  "PostStratifiedRatioEstimator",
  contains = "Estimator",
  slots = list(
    handler = "ANY",
    strata_weights = "ANY",
    var_est = "VarianceEstimator"
  )
)

#' Constructor for PostStratifiedRatioEstimator
#'
#' @param handler An optional EvalHandler object.
#' @param var_est A variance-estimator specification, or `"auto"` for the
#'   default ratio post-stratified variance estimator.
#' @export
PostStratifiedRatioEstimator <- function(handler = NULL, var_est = "auto") {
  var_est <- .resolve_post_strat_var_est(var_est = var_est, context = "ratio")

  if (!is.null(handler) && !inherits(handler, "EvalHandler")) {
    stop("`handler` must be NULL or an EvalHandler object.", call. = FALSE)
  }

  new(
    "PostStratifiedRatioEstimator",
    handler = handler,
    strata_weights = if (is.null(handler)) NULL else get_strata_weights(handler),
    var_est = var_est
  )
}

#' Estimate Ratio
#'
#' @param object A PostStratifiedRatioEstimator object.
#' @param intent A `fiaplyr_ratio_intent` object, usually created via
#'   `ratio(tree(...), cond(...))`.
#' @param domain_pairing Domain pairing strategy, either `"all"` (default) for
#'   all numerator/denominator domain combinations or `"matched"` to only retain
#'   rows where both sides share the same domain columns and values.
#' @param include_components Logical; if `TRUE`, append numerator and denominator
#'   component estimates and standard errors (`estimate_n`, `se_n`, `estimate_d`,
#'   `se_d`) to the output.
#' @export
setGeneric(
  "estimate_ratio",
  function(object, intent, domain_pairing = "all", include_components = FALSE) {
    standardGeneric("estimate_ratio")
  }
)

#' @describeIn estimate_ratio Estimate ratio for PostStratifiedRatioEstimator
setMethod(
  "estimate_ratio",
  "PostStratifiedRatioEstimator",
  function(
    object,
    intent,
    domain_pairing = c("all", "matched"),
    include_components = FALSE
  ) {
    if (is.null(object@handler)) {
      stop(
        "This estimator is not bound to a handler. Pass it as `estimate(handler, ratio(...), estimator = this_estimator)`.",
        call. = FALSE
      )
    }

    domain_pairing <- match.arg(domain_pairing)
    if (
      !is.logical(include_components) ||
        length(include_components) != 1 ||
        is.na(include_components)
    ) {
      stop("`include_components` must be TRUE or FALSE.")
    }
    if (!inherits(intent, "fiaplyr_ratio_intent")) {
      stop(
        "`intent` must be a `fiaplyr_ratio_intent` object, usually from `ratio(...)`."
      )
    }
    if (is.null(intent$numerator) || is.null(intent$denominator)) {
      stop(
        "`ratio()` must include both numerator and denominator target helpers."
      )
    }

    spec_num <- intent$numerator
    spec_den <- intent$denominator

    base_handler <- object@handler
    num_handler <- base_handler
    den_handler <- .psr_apply_den_partitions(
      base_handler,
      intent$den_partitions
    )

    # 1. Parse targets and aggregate plot-level data for each side
    parsed_num <- .parse_target_spec(spec_num, "estimate_ratio")
    parsed_den <- .parse_target_spec(spec_den, "estimate_ratio")

    agg_num <- .psr_aggregate(num_handler, parsed_num)
    agg_den <- .psr_aggregate(den_handler, parsed_den)

    # 2. Resolve value column names
    vals_num <- .psr_val_cols(parsed_num)
    vals_den <- .psr_val_cols(parsed_den)

    # 3. Join strata once for each side - reused by both the variance and covariance pipelines
    strata_num <- .ps_join_strata(agg_num, num_handler)
    strata_den <- .ps_join_strata(agg_den, den_handler)

    # 4. Stats pipeline for each side, producing [domain_vars, var, estimate, se]
    stats_num <- strata_num %>%
      .ps_strata_stats(vals_num) %>%
      .ps_eu_stats(vals_num) %>%
      .ps_pop_stats(num_handler, vals_num)

    stats_den <- strata_den %>%
      .ps_strata_stats(vals_den) %>%
      .ps_eu_stats(vals_den) %>%
      .ps_pop_stats(den_handler, vals_den)

    # 5. Covariance pipeline.
    # Build a lookup table mapping each cov column name to its (var_n, var_d) pair.
    # Row-major order: i (numerator) is the outer loop, j (denominator) is the inner loop.
    cov_pair_df <- data.frame(
      var_n = rep(vals_num, each = length(vals_den)),
      var_d = rep(vals_den, times = length(vals_num)),
      stringsAsFactors = FALSE
    )
    cov_cols <- paste0(".cov_", seq_len(nrow(cov_pair_df)))
    cov_pair_df$cov_col <- cov_cols

    pop_cov <- strata_num %>%
      .ps_strata_cov(strata_den, vals_num, vals_den, cov_cols) %>%
      .ps_eu_cov(cov_cols) %>%
      .ps_pop_cov(num_handler, cov_cols)

    # Reshape pop_cov to long format lazily using the known lookup. One lazy slice
    # per covariance column is stacked with union_all, avoiding a local pivot and
    # a lazy/local join (which dbplyr cannot do without copying).
    cov_domain_vars <- setdiff(colnames(pop_cov), cov_cols)
    cov_long_parts <- lapply(seq_len(nrow(cov_pair_df)), function(k) {
      cc <- rlang::sym(cov_pair_df$cov_col[k])
      pop_cov %>%
        dplyr::transmute(
          dplyr::across(dplyr::all_of(cov_domain_vars)),
          var_n = !!cov_pair_df$var_n[k],
          var_d = !!cov_pair_df$var_d[k],
          cov_val = !!cc
        )
    })
    pop_cov_long <- Reduce(dplyr::union_all, cov_long_parts)
    # Columns: [domain_vars_n, domain_vars_d, var_n, var_d, cov_val]

    # 6. Identify domain columns for each side
    doms_num <- setdiff(colnames(stats_num), c("var", "estimate", "se"))
    doms_den <- setdiff(colnames(stats_den), c("var", "estimate", "se"))
    .psr_validate_domain_pairing(domain_pairing, doms_num, doms_den)
    shared_doms <- intersect(doms_num, doms_den)
    doms_num_only <- setdiff(doms_num, shared_doms)
    doms_den_only <- setdiff(doms_den, shared_doms)

    # 7. Add _n/_d suffixes to domain cols and rename var/estimate/se, then cross-join
    suffix_n <- "_n"
    suffix_d <- "_d"

    stats_num_suf <- stats_num
    if (length(doms_num) > 0) {
      stats_num_suf <- stats_num_suf %>%
        dplyr::rename_with(~ paste0(.x, suffix_n), dplyr::all_of(doms_num))
    }
    stats_num_suf <- stats_num_suf %>%
      dplyr::rename(var_n = var, estimate_n = estimate, se_n = se)

    stats_den_suf <- stats_den
    if (length(doms_den) > 0) {
      stats_den_suf <- stats_den_suf %>%
        dplyr::rename_with(~ paste0(.x, suffix_d), dplyr::all_of(doms_den))
    }
    stats_den_suf <- stats_den_suf %>%
      dplyr::rename(var_d = var, estimate_d = estimate, se_d = se)

    # Pair numerator and denominator stats according to the requested domain strategy
    pop_joined <- .psr_join_stats(
      stats_num_suf,
      stats_den_suf,
      doms_num,
      doms_den,
      domain_pairing,
      suffix_n,
      suffix_d
    )

    # 8. Add _n/_d suffixes to pop_cov_long domain cols to match pop_joined, then join
    pop_cov_long_suf <- pop_cov_long
    if (length(doms_num_only) > 0) {
      pop_cov_long_suf <- pop_cov_long_suf %>%
        dplyr::rename_with(~ paste0(.x, suffix_n), dplyr::all_of(doms_num_only))
    }
    if (length(doms_den_only) > 0) {
      pop_cov_long_suf <- pop_cov_long_suf %>%
        dplyr::rename_with(~ paste0(.x, suffix_d), dplyr::all_of(doms_den_only))
    }
    if (length(shared_doms) > 0) {
      shared_num <- rlang::set_names(
        rlang::syms(shared_doms),
        paste0(shared_doms, suffix_n)
      )
      shared_den <- rlang::set_names(
        rlang::syms(shared_doms),
        paste0(shared_doms, suffix_d)
      )

      pop_cov_long_suf <- pop_cov_long_suf %>%
        dplyr::mutate(!!!shared_num, !!!shared_den) %>%
        dplyr::select(-dplyr::all_of(shared_doms))
    }

    doms_num_suf <- if (length(doms_num) > 0) {
      paste0(doms_num, suffix_n)
    } else {
      character(0)
    }
    doms_den_suf <- if (length(doms_den) > 0) {
      paste0(doms_den, suffix_d)
    } else {
      character(0)
    }

    cov_join_keys <- c(doms_num_suf, doms_den_suf, "var_n", "var_d")
    pop_full <- dplyr::left_join(
      pop_joined,
      pop_cov_long_suf,
      by = cov_join_keys
    )
    # Missing cov_val means numerator and denominator never co-occur on the same plot,
    # so all cross-products y_n * y_d = 0 and the true covariance is 0.
    pop_full <- pop_full %>%
      dplyr::mutate(cov_val = dplyr::coalesce(cov_val, 0))

    # 9. Apply the ratio variance formula:
    #    v(R) = (1/Y_d^2) * [v(Y_n) + R^2*v(Y_d) - 2*R*cov(Y_n, Y_d)]
    all_doms <- c(doms_num_suf, doms_den_suf)
    base_cols <- c(all_doms, "var_n", "var_d", "estimate", "se")
    component_cols <- c("estimate_n", "se_n", "estimate_d", "se_d")
    out_cols <- if (isTRUE(include_components)) {
      c(base_cols, component_cols)
    } else {
      base_cols
    }

    final_res <- pop_full %>%
      dplyr::mutate(
        estimate = estimate_n / estimate_d,
        var_ratio = (1 / estimate_d^2) *
          (se_n^2 +
            (estimate_n / estimate_d)^2 * se_d^2 -
            2 * (estimate_n / estimate_d) * cov_val),
        se = sqrt(pmax(var_ratio, 0))
      ) %>%
      dplyr::select(dplyr::all_of(out_cols))

    return(final_res)
  }
)


# --- PSR internal helpers ---

#' Apply denominator-only partition overrides for ratio intent
#' @noRd
.psr_apply_den_partitions <- function(handler, den_partitions) {
  if (is.null(den_partitions)) {
    return(handler)
  }

  parts <- den_partitions
  if (is.list(parts) && !is.null(attr(parts, "target_table"))) {
    parts <- list(parts)
  }

  if (!is.list(parts) || length(parts) == 0) {
    stop(
      "`den_partitions` must be NULL, a scoped helper, or a non-empty list of scoped helpers."
    )
  }

  is_scoped <- vapply(
    parts,
    function(x) {
      is.list(x) && !is.null(attr(x, "target_table"))
    },
    logical(1)
  )

  if (!all(is_scoped)) {
    stop(
      "All `den_partitions` entries must be scoped helpers like `cond(...)` or `tree(...)`."
    )
  }

  .route_scoped_expressions(handler, parts, operation = "set_domains")
}

#' Aggregate plot-level data for one side of the ratio
#' @noRd
.psr_aggregate <- function(handler, parsed) {
  if (parsed$slot == "tree") {
    if (
      length(parsed$targets) == 0 ||
        (length(parsed$targets) == 1 && parsed$targets == "1")
    ) {
      .make_tree_aggregates(handler, adjusted = TRUE, sparse = TRUE)
    } else {
      target_quos <- parsed$quosures
      if (is.null(target_quos)) {
        target_quos <- rlang::syms(parsed$targets)
      }
      names(target_quos) <- parsed$target_names
      .make_tree_aggregates(
        handler,
        !!!target_quos,
        adjusted = TRUE,
        sparse = TRUE
      )
    }
  } else if (parsed$slot == "tree_history") {
    if (
      length(parsed$targets) == 0 ||
        (length(parsed$targets) == 1 && parsed$targets == "1")
    ) {
      .make_tree_history_aggregates(handler, adjusted = TRUE, sparse = TRUE)
    } else {
      target_quos <- parsed$quosures
      if (is.null(target_quos)) {
        target_quos <- rlang::syms(parsed$targets)
      }
      names(target_quos) <- parsed$target_names
      .make_tree_history_aggregates(
        handler,
        !!!target_quos,
        adjusted = TRUE,
        sparse = TRUE
      )
    }
  } else if (parsed$slot == "cond") {
    cond_data <- .make_cond_aggregates(handler, adjusted = TRUE, sparse = TRUE)
    has_named_prop <- length(parsed$targets) > 0 &&
      all(parsed$targets == "1") &&
      length(parsed$target_names) == 1 &&
      nzchar(parsed$target_names[[1]])
    if (has_named_prop) {
      cond_data <- cond_data %>%
        dplyr::rename(!!parsed$target_names[[1]] := prop)
    }
    cond_data
  } else {
    stop("Unsupported slot: ", parsed$slot)
  }
}

#' Resolve value column names from parsed formula
#' @noRd
.psr_val_cols <- function(parsed) {
  if (
    length(parsed$targets) == 0 ||
      (length(parsed$targets) == 1 && parsed$targets == "1")
  ) {
    if (parsed$slot == "cond") {
      if (
        length(parsed$targets) == 1 &&
          length(parsed$target_names) == 1 &&
          nzchar(parsed$target_names[[1]])
      ) {
        parsed$target_names[[1]]
      } else {
        "prop"
      }
    } else {
      "tree_count"
    }
  } else {
    if (length(parsed$target_names) == length(parsed$targets)) {
      ifelse(nzchar(parsed$target_names), parsed$target_names, parsed$targets)
    } else {
      parsed$targets
    }
  }
}

#' Validate ratio-domain pairing mode
#' @noRd
.psr_validate_domain_pairing <- function(domain_pairing, doms_num, doms_den) {
  if (domain_pairing != "matched") {
    return(invisible(NULL))
  }

  if (!setequal(doms_num, doms_den)) {
    stop(
      "`domain_pairing = \"matched\"` requires numerator and denominator to have the same domain columns."
    )
  }

  invisible(NULL)
}

#' Pair numerator and denominator stats
#' @noRd
.psr_join_stats <- function(
  stats_num_suf,
  stats_den_suf,
  doms_num,
  doms_den,
  domain_pairing,
  suffix_n,
  suffix_d
) {
  if (domain_pairing != "matched" || length(doms_num) == 0) {
    return(dplyr::cross_join(stats_num_suf, stats_den_suf))
  }

  join_keys <- stats::setNames(
    paste0(doms_num, suffix_d),
    paste0(doms_num, suffix_n)
  )

  dplyr::inner_join(stats_num_suf, stats_den_suf, by = join_keys, keep = TRUE)
}

#' Run one side through the full post-stratification pipeline
#' @noRd
.psr_pop_estimate <- function(agg_data, handler, targets) {
  strata_data <- .ps_join_strata(agg_data, handler)
  strata_means <- .ps_strata_means(strata_data, targets)
  eu_data <- .ps_eu_estimates(strata_means, targets)
  .ps_pop_estimates(eu_data, handler, targets)
}
