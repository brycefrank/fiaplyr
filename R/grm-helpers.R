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
  
  # Mask everything outside the target transition and return as a fiaplyr_macro
  structure(
    list(
      expr = rlang::expr(ifelse(transition == !!transition_type, !!calc_expr, 0)),
      adjust = adjust,
      adjust_basis = adjust_basis,
      unknown_subptype = unknown_subptype
    ),
    class = "fiaplyr_macro"
  )
}

#' GRM Mortality Estimator
#'
#' Evaluates mortality variables at the midpoint, expanded by the initial
#' trees per acre.
#'
#' @inheritParams grm_macro_shared_args
#' @param expander The expansion factor. Defaults to `TPA_UNADJ_begin`.
#' @export
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

#' GRM Removals Estimator
#'
#' Evaluates removal variables at the midpoint, expanded by the initial
#' trees per acre.
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

#' GRM Ingrowth Estimator
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

#' GRM Survivor Estimator
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

#' GRM Reversion Estimator
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

#' GRM Diversion Estimator
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