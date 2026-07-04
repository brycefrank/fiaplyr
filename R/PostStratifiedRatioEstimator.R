#' PostStratifiedRatioEstimator Class
#'
#' @slot handler An EvalHandler object.
#' @slot strata_weights A dataframe containing strata weights.
#' @export
setClass(
  "PostStratifiedRatioEstimator",
  contains = "Estimator",
  slots = list(
    handler = "EvalHandler",
    strata_weights = "ANY"
  )
)

#' Constructor for PostStratifiedRatioEstimator
#'
#' @param handler An EvalHandler object.
#' @export
PostStratifiedRatioEstimator <- function(handler) {
  new(
    "PostStratifiedRatioEstimator",
    handler = handler,
    strata_weights = get_strata_weights(handler)
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

    parsed_num <- .parse_target_spec(spec_num, "estimate_ratio")
    parsed_den <- .parse_target_spec(spec_den, "estimate_ratio")

    # Get the attributes for each side
    atts_num <- .psr_val_cols(parsed_num)
    atts_den <- .psr_val_cols(parsed_den)

    # Aggregate the numerator
    agg_num <- aggregate(object@handler, spec_num, sparse = TRUE) |>
      dplyr::rename_with(~ paste0(.x, "_n"), dplyr::all_of(atts_num))

    # Aggregate the denominator, applying any den_partitions overrides
    agg_den <- aggregate(
      .psr_apply_den_partitions(object@handler, intent$den_partitions),
      spec_den,
      sparse = TRUE
    ) |>
      dplyr::rename_with(~ paste0(.x, "_d"), dplyr::all_of(atts_den))

    # Update the attribute names for later use with new suffixes
    atts_num_fmt <- paste0(atts_num, "_n")
    atts_den_fmt <- paste0(atts_den, "_d")
    atts_fmt <- c(atts_num_fmt, atts_den_fmt)

    doms_num <- setdiff(colnames(agg_num), c(.plot_keys, atts_num_fmt))
    doms_den <- setdiff(colnames(agg_den), c(.plot_keys, atts_den_fmt))
    .psr_validate_domain_pairing(domain_pairing, doms_num, doms_den)
    shared_doms <- intersect(doms_num, doms_den)

    # join the aggregations, suffixing _n and _d to side-specific columns
    agg <- dplyr::full_join(
      agg_num,
      agg_den,
      by = .plot_keys,
      suffix = c("_n", "_d")
    )

    if (domain_pairing == "matched" && length(shared_doms) > 0) {
      matched_checks <- lapply(shared_doms, function(dom) {
        lhs <- rlang::sym(paste0(dom, "_n"))
        rhs <- rlang::sym(paste0(dom, "_d"))
        rlang::expr((is.na(!!lhs) & is.na(!!rhs)) | (!!lhs == !!rhs))
      })

      agg <- agg %>% dplyr::filter(!!!matched_checks)
    }

    # 3. Join strata once for each side - reused by both the variance and covariance pipelines
    agg_strat <- .ps_join_strata(agg, object@handler)

    # 4. Stats pipeline for each side, producing [domain_vars, var, estimate, se]
    agg_stats <- agg_strat |>
      .ps_strata_stats(atts_fmt) |>
      .ps_eu_stats(atts_fmt) |>
      .ps_pop_stats(object@handler, atts_fmt)

    domain_cols <- setdiff(colnames(agg_stats), c("var", "estimate", "se"))

    num_map <- stats::setNames(atts_num, atts_num_fmt)
    den_map <- stats::setNames(atts_den, atts_den_fmt)

    num_case <- purrr::imap(num_map, function(out, inp) {
      rlang::expr(var == !!inp ~ !!out)
    })
    den_case <- purrr::imap(den_map, function(out, inp) {
      rlang::expr(var == !!inp ~ !!out)
    })

    # Split numerator and denominator
    stats_num <- agg_stats |>
      dplyr::filter(var %in% atts_num_fmt) |>
      dplyr::mutate(var_n = dplyr::case_when(!!!num_case)) |>
      dplyr::select(dplyr::all_of(domain_cols), var_n, estimate, se)

    stats_den <- agg_stats |>
      dplyr::filter(var %in% atts_den_fmt) |>
      dplyr::mutate(var_d = dplyr::case_when(!!!den_case)) |>
      dplyr::select(dplyr::all_of(domain_cols), var_d, estimate, se)

    stats <- dplyr::inner_join(
      stats_num,
      stats_den,
      by = domain_cols,
      suffix = c("_n", "_d")
    )

    stats

    # 5. Covariance pipeline on the unified strata table.
    #cov_pair_df <- data.frame(
    #  var_n = rep(atts_num, each = length(atts_den)),
    #  var_d = rep(atts_den, times = length(atts_num)),
    #  stringsAsFactors = FALSE
    #)
    #cov_cols <- paste0(".cov_", seq_len(nrow(cov_pair_df)))
    #cov_pair_df$cov_col <- cov_cols

    #cov_exprs <- list()
    #k <- 1L
    #for (i in seq_along(atts_num_fmt)) {
    #  for (j in seq_along(atts_den_fmt)) {
    #    x_n <- rlang::sym(atts_num_fmt[[i]])
    #    x_d <- rlang::sym(atts_den_fmt[[j]])
    #    cov_exprs[[cov_cols[[k]]]] <- rlang::expr(
    #      dplyr::case_when(
    #        n_h <= 1 ~ 0,
    #        TRUE ~ (sum(!!x_n * !!x_d, na.rm = TRUE) -
    #          sum(!!x_n, na.rm = TRUE) * sum(!!x_d, na.rm = TRUE) / n_h) /
    #          (n_h * (n_h - 1))
    #      )
    #    )
    #    k <- k + 1L
    #  }
    #}

    #strata_cov <- agg_strat |>
    #  dplyr::group_by(
    #    dplyr::across(
    #      dplyr::all_of(c(
    #        "ESTN_UNIT_CN",
    #        "STRATUM_CN",
    #        "w_h",
    #        "n_h",
    #        "n",
    #        domain_cols
    #      ))
    #    )
    #  ) |>
    #  dplyr::summarise(!!!cov_exprs) |>
    #  dplyr::ungroup()

    #pop_cov <- strata_cov %>%
    #  .ps_eu_cov(cov_cols) %>%
    #  .ps_pop_cov(object@handler, cov_cols)

    #pop_cov_long <- pop_cov %>%
    #  tidyr::pivot_longer(
    #    cols = dplyr::all_of(cov_cols),
    #    names_to = "cov_col",
    #    values_to = "cov_val"
    #  ) %>%
    #  dplyr::left_join(cov_pair_df, by = "cov_col") %>%
    #  dplyr::select(-cov_col)

    #cov_join_keys <- c(domain_cols, "var_n", "var_d")
    #pop_full <- dplyr::left_join(
    #  stats,
    #  pop_cov_long,
    #  by = cov_join_keys
    #)
    ## Missing cov_val means numerator and denominator never co-occur on the same plot,
    ## so all cross-products y_n * y_d = 0 and the true covariance is 0.
    #pop_full <- pop_full %>%
    #  dplyr::mutate(cov_val = dplyr::coalesce(cov_val, 0))

    ## 9. Apply the ratio variance formula:
    ##    v(R) = (1/Y_d^2) * [v(Y_n) + R^2*v(Y_d) - 2*R*cov(Y_n, Y_d)]
    #base_cols <- c(domain_cols, "var_n", "var_d", "estimate", "se")
    #component_cols <- c("estimate_n", "se_n", "estimate_d", "se_d")
    #out_cols <- if (isTRUE(include_components)) {
    #  c(base_cols, component_cols)
    #} else {
    #  base_cols
    #}

    #final_res <- pop_full %>%
    #  dplyr::mutate(
    #    estimate = estimate_n / estimate_d,
    #    var_ratio = (1 / estimate_d^2) *
    #      (se_n^2 +
    #        (estimate_n / estimate_d)^2 * se_d^2 -
    #        2 * (estimate_n / estimate_d) * cov_val),
    #    se = sqrt(pmax(var_ratio, 0))
    #  ) %>%
    #  dplyr::select(dplyr::all_of(out_cols))

    #return(final_res)
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
