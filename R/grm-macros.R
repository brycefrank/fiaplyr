# R/scoped-helpers-grm.R

#' Recursively append temporal suffixes to target symbols
#' @noRd
.append_suffix <- function(expr, suffix) {
  if (suffix == "" || is.null(expr)) return(expr)

  # If it's a bare variable name, append the suffix
  if (rlang::is_symbol(expr)) {
    sym_name <- rlang::as_string(expr)
    
    # Prevent double-suffixing if the user manually provided it
    if (grepl(paste0(suffix, "$"), sym_name)) return(expr)
    
    return(rlang::sym(paste0(sym_name, suffix)))
  }

  # If it's a mathematical operation/function, recurse into the arguments
  if (rlang::is_call(expr)) {
    call_args <- as.list(expr)[-1]
    expr[-1] <- lapply(call_args, .append_suffix, suffix = suffix)
    return(expr)
  }

  # Return constants (like 1) as-is
  expr
}

#' Shared GRM Macro Arguments
#'
#' Internal documentation anchor for shared GRM helper arguments.
#'
#' @param expr The variable to summarize. Use `1` for stem density.
#' @param annualize Logical. If `TRUE`, divides the estimate by `REMPER`.
#' @param adjust Adjustment behavior for macro-derived targets. One of
#'   `"auto"`, `"none"`, or `"subptype"`.
#' @param adjust_basis Basis used when `adjust = "subptype"`. Currently
#'   supported: `"subptyp_grm"`.
#' @param unknown_subptype Behavior when subtype cannot be mapped to an
#'   adjustment factor. One of `"zero"`, `"drop"`, or `"warn"`.
#' @name grm_macro_shared_args
NULL

#' Internal builder for GRM macros
#' @inheritParams grm_macro_shared_args
#' @noRd
.build_grm_macro <- function(
    transition_type,
    expr,
    expander,
    suffix = "",
    annualize = FALSE,
    adjust = "auto",
    adjust_basis = "subptyp_grm",
    unknown_subptype = "zero") {
  adjust <- match.arg(adjust, c("auto", "none", "subptype"))
  adjust_basis <- match.arg(adjust_basis, c("subptyp_grm"))
  unknown_subptype <- match.arg(unknown_subptype, c("zero", "drop", "warn"))

  expr_sym <- rlang::enexpr(expr)
  expander_sym <- rlang::enexpr(expander)
  
  # Inject the correct temporal suffix into the target expression
  suffixed_expr <- .append_suffix(expr_sym, suffix)
  
  # Handle implicit stem density when the user passes 1
  if (is.numeric(suffixed_expr) && suffixed_expr == 1) {
    calc_expr <- expander_sym
  } else {
    calc_expr <- rlang::expr((!!suffixed_expr) * (!!expander_sym))
  }
  
  # Inject the annualized rate logic if requested
  if (annualize) {
    calc_expr <- rlang::expr((!!calc_expr) / REMPER)
  }

  transition_aliases <- .grm_transition_aliases(transition_type)
  
  # Mask everything outside the target transition and return as a fiaplyr_macro
  structure(
    list(
      expr = rlang::expr(ifelse(transition %in% !!transition_aliases, !!calc_expr, 0)),
      adjust = adjust,
      adjust_basis = adjust_basis,
      unknown_subptype = unknown_subptype
    ),
    class = "fiaplyr_macro"
  )
}

#' @noRd
.grm_transition_aliases <- function(transition_type) {
  switch(
    transition_type,
    mortality = c("mortality", "mortality0", "mortality1", "mortality2"),
    # Treat condition-diversion removals as part of removals.
    removal = c("removal", "cut1", "cut2", "diversion1", "diversion2"),
    cut = c("cut", "cut1", "cut2"),
    harvest_removal = c("cut1", "cut2"),
    ingrowth = c("ingrowth"),
    survivor = c("survivor"),
    reversion = c("reversion", "reversion1", "reversion2"),
    diversion = c("diversion", "diversion0", "diversion1", "diversion2"),
    not_used = c("not_used"),
    transition_type
  )
}

#' @noRd
.build_grm_growth_delta_macro <- function(
    transition_type,
    expr,
    start_suffix,
    end_suffix,
    annualize = FALSE,
    adjust = "auto",
    adjust_basis = "subptyp_grm",
    unknown_subptype = "zero") {
  adjust <- match.arg(adjust, c("auto", "none", "subptype"))
  adjust_basis <- match.arg(adjust_basis, c("subptyp_grm"))
  unknown_subptype <- match.arg(unknown_subptype, c("zero", "drop", "warn"))

  expr_sym <- rlang::enexpr(expr)
  start_expr <- if (is.null(start_suffix)) rlang::expr(0) else .append_suffix(expr_sym, start_suffix)
  end_expr <- .append_suffix(expr_sym, end_suffix)

  delta_expr <- rlang::expr((!!end_expr) - (!!start_expr))

  if (annualize) {
    delta_expr <- rlang::expr((!!delta_expr) / REMPER)
  }

  transition_aliases <- .grm_transition_aliases(transition_type)

  structure(
    list(
      expr = rlang::expr(ifelse(transition %in% !!transition_aliases, !!delta_expr, 0)),
      adjust = adjust,
      adjust_basis = adjust_basis,
      unknown_subptype = unknown_subptype
    ),
    class = "fiaplyr_macro"
  )
}

#' @noRd
.combine_grm_macros <- function(macros, weights = NULL) {
  if (length(macros) == 0) {
    stop("Must provide at least one macro to combine.", call. = FALSE)
  }

  if (!all(vapply(macros, inherits, logical(1), what = "fiaplyr_macro"))) {
    stop("All inputs must be `fiaplyr_macro` objects.", call. = FALSE)
  }

  if (is.null(weights)) {
    weights <- rep(1, length(macros))
  }

  if (length(weights) != length(macros)) {
    stop("`weights` must have the same length as `macros`.", call. = FALSE)
  }

  weighted_terms <- Map(function(macro, weight) {
    if (identical(weight, 1)) {
      return(macro$expr)
    }
    rlang::expr((!!weight) * (!!macro$expr))
  }, macros, weights)

  combined_expr <- Reduce(function(lhs, rhs) rlang::expr((!!lhs) + (!!rhs)), weighted_terms)
  template <- macros[[1]]

  structure(
    list(
      expr = combined_expr,
      adjust = template$adjust,
      adjust_basis = template$adjust_basis,
      unknown_subptype = template$unknown_subptype
    ),
    class = "fiaplyr_macro"
  )
}

#' Specify a mortality macro
#'
#' This macro facilitates the implementation of mortality calculations in a
#' manner consistent with the GRM paradigm, including the appropriate use of
#' trees per acre expansion and temporal suffixes. Users supply the macro within
#' the `tree_history` context during `aggregate` or `estimate` operations.
#'
#' By default, the macro evaluates mortality at the midpoint of the measurement
#' interval expanded by the initial trees per acre. This follows the
#' specifications given in Bechtold and Patterson (2005). Users are able to
#' modify this behavior with macro arguments.
#'
#' @inheritParams grm_macro_shared_args @param expander The expansion factor.
#' Defaults to `TPA_UNADJ_begin`. @export
grm_mortality <- function(
    expr = 1,
    expander = TPA_UNADJ_begin,
    annualize = FALSE,
    adjust = "auto",
    adjust_basis = "subptyp_grm",
    unknown_subptype = "zero") {
  .build_grm_macro(
    "mortality",
    !!rlang::enexpr(expr),
    !!rlang::enexpr(expander),
    suffix = "_midpt",
    annualize = annualize,
    adjust = adjust,
    adjust_basis = adjust_basis,
    unknown_subptype = unknown_subptype
  )
}

#' Specify a removals macro
#'
#' This macro facilitates the implementation of removal calculations in a
#' manner consistent with the GRM paradigm, including the appropriate use of
#' trees per acre expansion and temporal suffixes. Users supply the macro within
#' the `tree_history` context during `aggregate` or `estimate` operations.
#'
#' By default, the macro evaluates removals at the midpoint of the measurement
#' interval expanded by the initial trees per acre. This follows the
#' specifications given in Bechtold and Patterson (2005). Users are able to
#' modify this behavior with macro arguments.
#'
#' @inheritParams grm_macro_shared_args
#' @param expander The expansion factor. Defaults to `TPA_UNADJ_begin`.
#' @export
grm_removals <- function(
    expr = 1,
    expander = TPA_UNADJ_begin,
    annualize = FALSE,
    adjust = "auto",
    adjust_basis = "subptyp_grm",
    unknown_subptype = "zero") {
  .build_grm_macro(
    "removal",
    !!rlang::enexpr(expr),
    !!rlang::enexpr(expander),
    suffix = "_midpt",
    annualize = annualize,
    adjust = adjust,
    adjust_basis = adjust_basis,
    unknown_subptype = unknown_subptype
  )
}

#' Specify a survivor growth macro
#'
#' @inheritParams grm_macro_shared_args
#' @export
grm_growth_survivor <- function(
    expr = 1,
    annualize = FALSE,
    adjust = "auto",
    adjust_basis = "subptyp_grm",
    unknown_subptype = "zero") {
  .build_grm_growth_delta_macro(
    "survivor",
    !!rlang::enexpr(expr),
    start_suffix = "_begin",
    end_suffix = "",
    annualize = annualize,
    adjust = adjust,
    adjust_basis = adjust_basis,
    unknown_subptype = unknown_subptype
  )
}

#' Specify an ingrowth growth macro
#'
#' @inheritParams grm_macro_shared_args
#' @export
grm_growth_ingrowth <- function(
    expr = 1,
    annualize = FALSE,
    adjust = "auto",
    adjust_basis = "subptyp_grm",
    unknown_subptype = "zero") {
  .build_grm_growth_delta_macro(
    "ingrowth",
    !!rlang::enexpr(expr),
    start_suffix = NULL,
    end_suffix = "",
    annualize = annualize,
    adjust = adjust,
    adjust_basis = adjust_basis,
    unknown_subptype = unknown_subptype
  )
}

#' Specify a reversion growth macro
#'
#' @inheritParams grm_macro_shared_args
#' @export
grm_growth_reversion <- function(
    expr = 1,
    annualize = FALSE,
    adjust = "auto",
    adjust_basis = "subptyp_grm",
    unknown_subptype = "zero") {
  .build_grm_growth_delta_macro(
    "reversion",
    !!rlang::enexpr(expr),
    start_suffix = "_midpt",
    end_suffix = "",
    annualize = annualize,
    adjust = adjust,
    adjust_basis = adjust_basis,
    unknown_subptype = unknown_subptype
  )
}

#' Specify a mortality growth macro
#'
#' @inheritParams grm_macro_shared_args
#' @export
grm_growth_mortality <- function(
    expr = 1,
    annualize = FALSE,
    adjust = "auto",
    adjust_basis = "subptyp_grm",
    unknown_subptype = "zero") {
  .build_grm_growth_delta_macro(
    "mortality",
    !!rlang::enexpr(expr),
    start_suffix = "_begin",
    end_suffix = "_midpt",
    annualize = annualize,
    adjust = adjust,
    adjust_basis = adjust_basis,
    unknown_subptype = unknown_subptype
  )
}

#' Specify a cut growth macro
#'
#' @inheritParams grm_macro_shared_args
#' @export
grm_growth_cut <- function(
    expr = 1,
    annualize = FALSE,
    adjust = "auto",
    adjust_basis = "subptyp_grm",
    unknown_subptype = "zero") {
  .build_grm_growth_delta_macro(
    "cut",
    !!rlang::enexpr(expr),
    start_suffix = "_begin",
    end_suffix = "_midpt",
    annualize = annualize,
    adjust = adjust,
    adjust_basis = adjust_basis,
    unknown_subptype = unknown_subptype
  )
}

#' Specify a diversion growth macro
#'
#' @inheritParams grm_macro_shared_args
#' @export
grm_growth_diversion <- function(
    expr = 1,
    annualize = FALSE,
    adjust = "auto",
    adjust_basis = "subptyp_grm",
    unknown_subptype = "zero") {
  .build_grm_growth_delta_macro(
    "diversion",
    !!rlang::enexpr(expr),
    start_suffix = "_begin",
    end_suffix = "_midpt",
    annualize = annualize,
    adjust = adjust,
    adjust_basis = adjust_basis,
    unknown_subptype = unknown_subptype
  )
}

#' Specify a gross ingrowth macro
#'
#' Gross ingrowth is defined as ingrowth plus reversion ($I + R$).
#'
#' @inheritParams grm_macro_shared_args
#' @export
grm_gross_ingrowth <- function(
    expr = 1,
    annualize = FALSE,
    adjust = "auto",
    adjust_basis = "subptyp_grm",
    unknown_subptype = "zero") {
  expr_sym <- rlang::enexpr(expr)

  ingrowth <- grm_growth_ingrowth(
    !!expr_sym,
    annualize = annualize,
    adjust = adjust,
    adjust_basis = adjust_basis,
    unknown_subptype = unknown_subptype
  )

  reversion <- grm_growth_reversion(
    !!expr_sym,
    annualize = annualize,
    adjust = adjust,
    adjust_basis = adjust_basis,
    unknown_subptype = unknown_subptype
  )

  .combine_grm_macros(list(ingrowth, reversion))
}

#' Specify an accretion macro
#'
#' Accretion is defined as $GS + GI + GR + GM + GC + GD$.
#'
#' @inheritParams grm_macro_shared_args
#' @export
grm_accretion <- function(
    expr = 1,
    annualize = FALSE,
    adjust = "auto",
    adjust_basis = "subptyp_grm",
    unknown_subptype = "zero") {
  expr_sym <- rlang::enexpr(expr)

  survivor <- grm_growth_survivor(
    !!expr_sym,
    annualize = annualize,
    adjust = adjust,
    adjust_basis = adjust_basis,
    unknown_subptype = unknown_subptype
  )

  ingrowth <- grm_growth_ingrowth(
    !!expr_sym,
    annualize = annualize,
    adjust = adjust,
    adjust_basis = adjust_basis,
    unknown_subptype = unknown_subptype
  )

  reversion <- grm_growth_reversion(
    !!expr_sym,
    annualize = annualize,
    adjust = adjust,
    adjust_basis = adjust_basis,
    unknown_subptype = unknown_subptype
  )

  mortality <- grm_growth_mortality(
    !!expr_sym,
    annualize = annualize,
    adjust = adjust,
    adjust_basis = adjust_basis,
    unknown_subptype = unknown_subptype
  )

  cut <- grm_growth_cut(
    !!expr_sym,
    annualize = annualize,
    adjust = adjust,
    adjust_basis = adjust_basis,
    unknown_subptype = unknown_subptype
  )

  diversion <- grm_growth_diversion(
    !!expr_sym,
    annualize = annualize,
    adjust = adjust,
    adjust_basis = adjust_basis,
    unknown_subptype = unknown_subptype
  )

  .combine_grm_macros(list(survivor, ingrowth, reversion, mortality, cut, diversion))
}

#' Specify a gross growth macro
#'
#' Gross growth is defined as gross ingrowth plus accretion.
#'
#' @inheritParams grm_macro_shared_args
#' @export
grm_gross_growth <- function(
    expr = 1,
    annualize = FALSE,
    adjust = "auto",
    adjust_basis = "subptyp_grm",
    unknown_subptype = "zero") {
  expr_sym <- rlang::enexpr(expr)

  gross_ingrowth <- grm_gross_ingrowth(
    !!expr_sym,
    annualize = annualize,
    adjust = adjust,
    adjust_basis = adjust_basis,
    unknown_subptype = unknown_subptype
  )

  accretion <- grm_accretion(
    !!expr_sym,
    annualize = annualize,
    adjust = adjust,
    adjust_basis = adjust_basis,
    unknown_subptype = unknown_subptype
  )

  .combine_grm_macros(list(gross_ingrowth, accretion))
}

#' Specify a gross growth macro
#'
#' Compatibility alias for [grm_gross_growth()].
#'
#' @inheritParams grm_gross_growth
#' @export
grom_gross_growth <- function(
    expr = 1,
    annualize = FALSE,
    adjust = "auto",
    adjust_basis = "subptyp_grm",
    unknown_subptype = "zero") {
  grm_gross_growth(
    expr = !!rlang::enexpr(expr),
    annualize = annualize,
    adjust = adjust,
    adjust_basis = adjust_basis,
    unknown_subptype = unknown_subptype
  )
}

#' Specify a net growth macro
#'
#' Net growth is defined as gross growth minus mortality.
#'
#' @inheritParams grm_macro_shared_args
#' @export
grm_net_growth <- function(
    expr = 1,
    annualize = FALSE,
    adjust = "auto",
    adjust_basis = "subptyp_grm",
    unknown_subptype = "zero") {
  expr_sym <- rlang::enexpr(expr)

  gross_growth <- grm_gross_growth(
    !!expr_sym,
    annualize = annualize,
    adjust = adjust,
    adjust_basis = adjust_basis,
    unknown_subptype = unknown_subptype
  )

  mortality <- grm_mortality(
    !!expr_sym,
    annualize = annualize,
    adjust = adjust,
    adjust_basis = adjust_basis,
    unknown_subptype = unknown_subptype
  )

  .combine_grm_macros(list(gross_growth, mortality), weights = c(1, -1))
}

#' Specify a net change macro
#'
#' Net change is defined as net growth minus removals.
#'
#' @inheritParams grm_macro_shared_args
#' @export
grm_net_change <- function(
    expr = 1,
    annualize = FALSE,
    adjust = "auto",
    adjust_basis = "subptyp_grm",
    unknown_subptype = "zero") {
  expr_sym <- rlang::enexpr(expr)

  net_growth <- grm_net_growth(
    !!expr_sym,
    annualize = annualize,
    adjust = adjust,
    adjust_basis = adjust_basis,
    unknown_subptype = unknown_subptype
  )

  removals <- grm_removals(
    !!expr_sym,
    annualize = annualize,
    adjust = adjust,
    adjust_basis = adjust_basis,
    unknown_subptype = unknown_subptype
  )

  .combine_grm_macros(list(net_growth, removals), weights = c(1, -1))
}

#' Specify a harvest removals macro
#'
#' This macro facilitates the implementation of harvest-removal calculations
#' using only cut1 and cut2 transitions.
#'
#' @inheritParams grm_macro_shared_args
#' @param expander The expansion factor. Defaults to `TPA_UNADJ_begin`.
#' @export
grm_harvest_removal <- function(
    expr = 1,
    expander = TPA_UNADJ_begin,
    annualize = FALSE,
    adjust = "auto",
    adjust_basis = "subptyp_grm",
    unknown_subptype = "zero") {
  .build_grm_macro(
    "harvest_removal",
    !!rlang::enexpr(expr),
    !!rlang::enexpr(expander),
    suffix = "_midpt",
    annualize = annualize,
    adjust = adjust,
    adjust_basis = adjust_basis,
    unknown_subptype = unknown_subptype
  )
}

#' Specify an ingrowth macro
#'
#' This macro facilitates the implementation of ingrowth calculations in a
#' manner consistent with the GRM paradigm, including the appropriate use of
#' trees per acre expansion and temporal suffixes. Users supply the macro within
#' the `tree_history` context during `aggregate` or `estimate` operations.
#'j
#' By default, the macro evaluates ingrowth at the midpoint of the measurement
#' interval expanded by the initial trees per acre. This follows the
#' specifications given in Bechtold and Patterson (2005). Users are able to
#' modify this behavior with macro arguments.
#'
#'
#' Evaluates ingrowth variables at the measurement endpoint, expanded by the
#' current trees per acre.
#'
#' @inheritParams grm_macro_shared_args
#' @param expander The expansion factor. Defaults to `TPA_UNADJ`.
#' @export
grm_ingrowth <- function(
    expr = 1,
    expander = TPA_UNADJ,
    annualize = FALSE,
    adjust = "auto",
    adjust_basis = "subptyp_grm",
    unknown_subptype = "zero") {
  .build_grm_macro(
    "ingrowth",
    !!rlang::enexpr(expr),
    !!rlang::enexpr(expander),
    suffix = "",
    annualize = annualize,
    adjust = adjust,
    adjust_basis = adjust_basis,
    unknown_subptype = unknown_subptype
  )
}

#' Specify a survivor macro
#'
#' @inheritParams grm_macro_shared_args
#' @param expander The expansion factor. Defaults to `TPA_UNADJ`.
#' @export
grm_survivor <- function(
    expr = 1,
    expander = TPA_UNADJ,
    annualize = FALSE,
    adjust = "auto",
    adjust_basis = "subptyp_grm",
    unknown_subptype = "zero") {
  .build_grm_macro(
    "survivor",
    !!rlang::enexpr(expr),
    !!rlang::enexpr(expander),
    suffix = "",
    annualize = annualize,
    adjust = adjust,
    adjust_basis = adjust_basis,
    unknown_subptype = unknown_subptype
  )
}

#' Specify a reversion macro
#'
#' @inheritParams grm_macro_shared_args
#' @param expander The expansion factor. Defaults to `TPA_UNADJ`.
#' @export
grm_reversion <- function(
    expr = 1,
    expander = TPA_UNADJ,
    annualize = FALSE,
    adjust = "auto",
    adjust_basis = "subptyp_grm",
    unknown_subptype = "zero") {
  .build_grm_macro(
    "reversion",
    !!rlang::enexpr(expr),
    !!rlang::enexpr(expander),
    suffix = "",
    annualize = annualize,
    adjust = adjust,
    adjust_basis = adjust_basis,
    unknown_subptype = unknown_subptype
  )
}

#' Specify a diversion macro
#'
#' Evaluates diversion variables at the measurement beginning, expanded by the
#' initial trees per acre.
#'
#' @inheritParams grm_macro_shared_args
#' @param expander The expansion factor. Defaults to `TPA_UNADJ_begin`.
#' @export
grm_diversion <- function(
    expr = 1,
    expander = TPA_UNADJ_begin,
    annualize = FALSE,
    adjust = "auto",
    adjust_basis = "subptyp_grm",
    unknown_subptype = "zero") {
  .build_grm_macro(
    "diversion",
    !!rlang::enexpr(expr),
    !!rlang::enexpr(expander),
    suffix = "_begin",
    annualize = annualize,
    adjust = adjust,
    adjust_basis = adjust_basis,
    unknown_subptype = unknown_subptype
  )
}