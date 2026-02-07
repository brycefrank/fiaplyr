#' Parse Estimation Formula
#'
#' Parses a formula of the form `slot ~ variable | variable2` into its
#' components.
#'
#' @param f A formula.
#' @return A list containing the slot name and a character vector of target
#'  variables.
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
      return(as.character(expr)) # This might fail for complex expressions, but keeping simple for now
    }
  }

  targets <- collect_vars(rhs)

  list(
    slot = slot_name,
    targets = targets
  )
}
