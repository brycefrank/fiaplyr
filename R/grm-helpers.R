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

#' Internal builder for GRM macros
#' @noRd
.build_grm_macro <- function(transition_type, expr, expander, suffix = "", annualize = FALSE) {
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
  
  # Mask everything outside the target transition
  rlang::expr(
    ifelse(transition == !!transition_type, !!calc_expr, 0)
  )
}

#' GRM Mortality Estimator
#'
#' Evaluates mortality variables at the midpoint, expanded by the initial
#' trees per acre.
#'
#' @param expr The variable to summarize. Defaults to 1 (stem density).
#' @param expander The expansion factor. Defaults to `TPA_UNADJ_begin`.
#' @param annualize Logical. If `TRUE`, divides the result by `REMPER` to calculate an annual rate.
#' @export
grm_mortality <- function(expr = 1, expander = TPA_UNADJ_begin, annualize = FALSE) {
  .build_grm_macro("mortality", !!rlang::enexpr(expr), !!rlang::enexpr(expander), suffix = "_midpt", annualize = annualize)
}

#' GRM Removals Estimator
#'
#' Evaluates removal variables at the midpoint, expanded by the initial
#' trees per acre.
#' 
#' @param expr The variable to summarize. Defaults to 1 (stem density).
#' @param expander The expansion factor. Defaults to `TPA_UNADJ_begin`.
#' @param annualize Logical. If `TRUE`, divides the result by `REMPER` to calculate an annual rate.
#' @export
grm_removals <- function(expr = 1, expander = TPA_UNADJ_begin, annualize = FALSE) {
  .build_grm_macro("removal", !!rlang::enexpr(expr), !!rlang::enexpr(expander), suffix = "_midpt", annualize = annualize)
}

#' GRM Ingrowth Estimator
#'
#' Evaluates ingrowth variables at the measurement endpoint, expanded by the
#' current trees per acre.
#'
#' @param expr The variable to summarize. Defaults to 1 (stem density).
#' @param expander The expansion factor. Defaults to `TPA_UNADJ`.
#' @param annualize Logical. If `TRUE`, divides the result by `REMPER` to calculate an annual rate.
#' @export
grm_ingrowth <- function(expr = 1, expander = TPA_UNADJ, annualize = FALSE) {
  .build_grm_macro("ingrowth", !!rlang::enexpr(expr), !!rlang::enexpr(expander), suffix = "", annualize = annualize)
}

#' GRM Survivor Estimator
#'
#' @param expr The variable to summarize. Defaults to 1 (stem density).
#' @param expander The expansion factor. Defaults to `TPA_UNADJ`.
#' @param annualize Logical. If `TRUE`, divides the result by `REMPER` to calculate an annual rate.
#' @export
grm_survivor <- function(expr = 1, expander = TPA_UNADJ, annualize = FALSE) {
  .build_grm_macro("survivor", !!rlang::enexpr(expr), !!rlang::enexpr(expander), suffix = "", annualize = annualize)
}

#' GRM Reversion Estimator
#'
#' @param expr The variable to summarize. Defaults to 1 (stem density).
#' @param expander The expansion factor. Defaults to `TPA_UNADJ`.
#' @param annualize Logical. If `TRUE`, divides the result by `REMPER` to calculate an annual rate.
#' @export
grm_reversion <- function(expr = 1, expander = TPA_UNADJ, annualize = FALSE) {
  .build_grm_macro("reversion", !!rlang::enexpr(expr), !!rlang::enexpr(expander), suffix = "", annualize = annualize)
}

#' GRM Diversion Estimator
#'
#' Evaluates diversion variables at the measurement beginning, expanded by the
#' initial trees per acre.
#'
#' @param expr The variable to summarize. Defaults to 1 (stem density).
#' @param expander The expansion factor. Defaults to `TPA_UNADJ_begin`.
#' @param annualize Logical. If `TRUE`, divides the result by `REMPER` to calculate an annual rate.
#' @export
grm_diversion <- function(expr = 1, expander = TPA_UNADJ_begin, annualize = FALSE) {
  .build_grm_macro("diversion", !!rlang::enexpr(expr), !!rlang::enexpr(expander), suffix = "_begin", annualize = annualize)
}