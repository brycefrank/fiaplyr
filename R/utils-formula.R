#' Parse Estimation Formula
#'
#' Parses a formula of the form `slot ~ variable | variable2` into its components.
#'
#' @param f A formula.
#' @return A list containing the slot name and a character vector of target variables.
#' @export
#' @examples
#' parse_formula(tree ~ VOLCFGRS | VOLCFNET)
#' parse_formula(cond ~ 1)
parse_formula <- function(f) {
  if (!inherits(f, "formula")) {
    stop("Input must be a formula.")
  }

  if (length(f) != 3) {
    stop("Formula must be of the form LHS ~ RHS.")
  }

  # Extract LHS (slot)
  lhs <- f[[2]]
  if (!is.symbol(lhs)) {
    stop("LHS of formula must be a symbol (slot name).")
  }
  slot_name <- as.character(lhs)

  # Extract RHS (targets)
  rhs <- f[[3]]

  # Helper to collect variables separated by |
  collect_vars <- function(expr) {
    if (length(expr) == 1) {
      return(as.character(expr))
    } else if (as.character(expr[[1]]) == "|") {
      return(c(collect_vars(expr[[2]]), collect_vars(expr[[3]])))
    } else {
      # If it's effectively a single term (like a function call or just a symbol not separated by |), treat as one
      # But wait, user said "slot ~ variable_1 | variable_2"
      # If it is `grouping` type syntax, `|` is special.
      # If the user passes `tree ~ VOLCFGRS`, expr is just `VOLCFGRS` (symbol).
      # If `tree ~ VOLCFGRS | VOLCFNET`, expr is `|(VOLCFGRS, VOLCFNET)` (call).
      return(as.character(expr)) # This might fail for complex expressions, but keeping simple for now
    }
  }

  # For `tree ~ VOLCFGRS | VOLCFNET`, the AST is `|`(VOLCFGRS, VOLCFNET)
  # But `|` is binary. `a | b | c` is `|`(`|`(a, b), c) or similar.

  targets <- collect_vars(rhs)

  list(
    slot = slot_name,
    targets = targets
  )
}

#' Parse Ratio Estimation Formula
#'
#' Parses a formula of the form `slot ~ num / den | num2 / den2`.
#'
#' @param f A formula.
#' @return A list containing the slot name and a list of ratios.
#' @export
parse_ratio_formula <- function(f) {
  if (!inherits(f, "formula")) {
    stop("Input must be a formula.")
  }

  if (length(f) != 3) {
    stop("Formula must be of the form LHS ~ RHS.")
  }

  # Extract LHS (slot)
  lhs <- f[[2]]
  if (!is.symbol(lhs)) {
    stop("LHS of formula must be a symbol (slot name).")
  }
  slot_name <- as.character(lhs)

  # Extract RHS (ratios)
  rhs <- f[[3]]

  # Helper to collect ratios separated by |
  collect_ratios <- function(expr) {
    if (length(expr) == 1) {
      return(list(expr))
    } else if (as.character(expr[[1]]) == "|") {
      return(c(collect_ratios(expr[[2]]), collect_ratios(expr[[3]])))
    } else {
      return(list(expr))
    }
  }

  ratio_exprs <- collect_ratios(rhs)

  parsed_ratios <- lapply(ratio_exprs, function(expr) {
    if (length(expr) == 3 && as.character(expr[[1]]) == "/") {
      num <- expr[[2]]
      den <- expr[[3]]

      if (!is.symbol(num) || !is.symbol(den)) {
         stop("Numerator and denominator must be variable names.")
      }

      list(
        numerator = as.character(num),
        denominator = as.character(den)
      )
    } else {
      stop("Ratio must be defined as Numerator / Denominator.")
    }
  })

  list(
    slot = slot_name,
    ratios = parsed_ratios
  )
}
