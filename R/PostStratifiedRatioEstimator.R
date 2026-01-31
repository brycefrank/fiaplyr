#' PostStratifiedRatioEstimator Class
#'
#' @slot numerator A EvalHandler object for the numerator.
#' @slot denominator A EvalHandler object for the denominator.
#' @slot strata_weights A dataframe containing strata weights.
#' @export
setClass("PostStratifiedRatioEstimator",
  contains = "Estimator",
  slots = list(
    numerator = "EvalHandler",
    denominator = "EvalHandler",
    strata_weights = "ANY"
  )
)

#' Constructor for PostStratifiedRatioEstimator
#'
#' @param numerator A EvalHandler object for the numerator.
#' @param denominator A EvalHandler object for the denominator. Defaults to numerator.
#' @export
PostStratifiedRatioEstimator <- function(numerator, denominator = numerator) {
  # Check if EVALIDs match
  if (evalid(numerator) != evalid(denominator)) {
    stop("Numerator and denominator must have the same EVALID.")
  }

  # Calculate strata weights (using numerator, as they share EVALID)
  strata_weights <- numerator@tables$pop_stratum %>%
    dplyr::inner_join(
      numerator@tables$pop_estn_unit,
      by = c("ESTN_UNIT_CN" = "CN"),
      suffix = c("", ".eu")
    ) %>%
    dplyr::mutate(
      w_h = as.numeric(P1POINTCNT) / P1PNTCNT_EU
    ) %>%
    dplyr::select(
      STRATUM_CN = CN, ESTN_UNIT_CN, w_h, P2POINTCNT, AREA_USED
    )

  new("PostStratifiedRatioEstimator",
    numerator = numerator,
    denominator = denominator,
    strata_weights = strata_weights
  )
}

#' Estimate Ratio
#'
#' @param object A PostStratifiedRatioEstimator object.
#' @param ... Ratio formulas.
#' @export
setGeneric("estimate_ratio", function(object, ...) standardGeneric("estimate_ratio"))

#' @describeIn estimate_ratio Estimate ratio for PostStratifiedRatioEstimator
setMethod("estimate_ratio", "PostStratifiedRatioEstimator", function(object, ...) {
  args <- list(...)
  if (length(args) != 2) stop("Must provide exactly two formulas (numerator, denominator).")

  f_num <- args[[1]]
  f_den <- args[[2]]

  # 1. Aggregate Numerator and Denominator
  # --------------------------------------
  agg_num_lazy <- aggregate(object@numerator, f_num)
  agg_den_lazy <- aggregate(object@denominator, f_den)

  dat_num <- dplyr::collect(agg_num_lazy)
  dat_den <- dplyr::collect(agg_den_lazy)

  # 2. Identify Columns (Keys, Domains, Values)
  # -------------------------------------------
  plot_keys <- c("PLT_CN", "STATECD", "INVYR", "PLOT", "COUNTYCD")

  parsed_num <- parse_formula(f_num)
  parsed_den <- parse_formula(f_den)

  vals_num <- parsed_num$targets
  vals_den <- parsed_den$targets

  if (length(vals_num) == 1 && vals_num == "1") vals_num <- "1"
  if (length(vals_den) == 1 && vals_den == "1") vals_den <- "1"

  cols_num <- colnames(dat_num)
  doms_num <- setdiff(cols_num, c(plot_keys, vals_num))

  cols_den <- colnames(dat_den)
  doms_den <- setdiff(cols_den, c(plot_keys, vals_den))

  # 3. Pivot to Wide Format
  # -----------------------
  p_num <- .process_wide(dat_num, doms_num, vals_num, "N")
  p_den <- .process_wide(dat_den, doms_den, vals_den, "D")

  # 4. Join and Attach Weights
  # --------------------------
  joined <- dplyr::full_join(p_num$wide, p_den$wide, by = "PLT_CN")

  data_cols <- c(p_num$meta$col_name, p_den$meta$col_name)
  joined <- joined %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(data_cols), ~ tidyr::replace_na(., 0)))

  plt_cns <- joined$PLT_CN

  plot_stratum <- object@numerator@tables$pop_plot_stratum_assgn %>%
    dplyr::collect() %>%
    dplyr::select(PLT_CN, STRATUM_CN)

  joined_strat <- joined %>%
    dplyr::inner_join(plot_stratum, by = "PLT_CN")

  strata_weights <- dplyr::collect(object@strata_weights)

  final_dat <- joined_strat %>%
    dplyr::inner_join(strata_weights, by = "STRATUM_CN")

  # 5. Estimate Totals (Means)
  # --------------------------
  strat_means <- final_dat %>%
    dplyr::group_by(ESTN_UNIT_CN, STRATUM_CN, w_h, P2POINTCNT) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(data_cols), ~ sum(.x) / unique(P2POINTCNT)),
      .groups = "drop"
    )

  eu_means <- strat_means %>%
    dplyr::group_by(ESTN_UNIT_CN) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(data_cols), ~ sum(.x * w_h)),
      .groups = "drop"
    )

  eu_weights <- object@numerator@tables$pop_estn_unit %>%
    dplyr::collect() %>%
    dplyr::mutate(w_eu = P1PNTCNT_EU / sum(P1PNTCNT_EU, na.rm = TRUE)) %>%
    dplyr::select(ESTN_UNIT_CN = CN, w_eu)

  pop_est <- eu_means %>%
    dplyr::left_join(eu_weights, by = "ESTN_UNIT_CN") %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(data_cols), ~ sum(.x * w_eu, na.rm = TRUE))
    )

  # 6. Compute Ratios
  # -----------------
  vals <- as.list(pop_est)

  res_grid <- tidyr::expand_grid(
    num_col = p_num$meta$col_name,
    den_col = p_den$meta$col_name
  )

  res_annotated <- res_grid %>%
    dplyr::left_join(p_num$meta, by = c("num_col" = "col_name")) %>%
    dplyr::left_join(p_den$meta, by = c("den_col" = "col_name"), suffix = c("", "_den"))

  res_annotated$estimate <- mapply(function(n, d) {
    vn <- vals[[n]]
    vd <- vals[[d]]
    if (vd == 0) return(NA_real_)
    vn / vd
  }, res_annotated$num_col, res_annotated$den_col)

  final_res <- res_annotated %>%
    dplyr::select(-num_col, -den_col)

  return(final_res)
})


#' Helper to process data into wide format with metadata
#' @keywords internal
.process_wide <- function(dat, doms, vals, prefix) {
  if (length(doms) == 0) {
    wide <- dat %>%
      dplyr::select(dplyr::all_of(c("PLT_CN", vals))) %>%
      dplyr::rename_with(~ paste0(prefix, "_", .), .cols = dplyr::all_of(vals))

    meta <- tidyr::expand_grid(variable = vals) %>%
      dplyr::mutate(col_name = paste0(prefix, "_", variable))

    return(list(wide = wide, meta = meta))
  }

  unique_doms <- dat %>%
    dplyr::select(dplyr::all_of(doms)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(dom_id = dplyr::row_number())

  dat_with_id <- dat %>%
    dplyr::inner_join(unique_doms, by = doms)

  wide <- dat_with_id %>%
    dplyr::select(PLT_CN, dom_id, dplyr::all_of(vals)) %>%
    tidyr::pivot_wider(
      id_cols = PLT_CN,
      names_from = dom_id,
      values_from = dplyr::all_of(vals),
      names_glue = "{.value}_dom{dom_id}",
      values_fill = 0
    )

  wide_cols <- setdiff(colnames(wide), "PLT_CN")

  matches <- regexec("^(.*)_dom(\\d+)$", wide_cols)
  extracted <- regmatches(wide_cols, matches)

  meta_list <- lapply(seq_along(wide_cols), function(i) {
    parts <- extracted[[i]]
    var_name <- parts[2]
    d_id <- as.integer(parts[3])

    d_vals <- unique_doms %>% dplyr::filter(dom_id == !!d_id)

    cbind(
      data.frame(col_name = wide_cols[i], variable = var_name, stringsAsFactors = FALSE),
      d_vals %>% dplyr::select(-dom_id)
    )
  })

  meta <- do.call(rbind, meta_list)

  new_names <- paste0(prefix, "_", wide_cols)
  colnames(wide)[match(wide_cols, colnames(wide))] <- new_names
  meta$col_name <- new_names

  list(wide = wide, meta = meta)
}
