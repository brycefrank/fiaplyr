#' Scoped Helper for Tree-Level Expressions
#'
#' Captures one or more expressions and tags them to be applied at the tree
#' table level during lazy evaluation. Used with `transform()`, `subset()`,
#' or `partition()` to explicitly scope mutations, filters, or domain
#' variables.
#'
#' @param ... Zero or more named or unnamed expressions.
#' @return A list of quosures tagged with `target_table = "tree"`.
#' @export
#' @examples
#' \dontrun{
#'   handler |>
#'     transform(tree(BA = 0.005454 * DIA^2)) |>
#'     subset(tree(STATUSCD == 1)) |>
#'     partition(tree(SPCD))
#' }
tree <- function(...) {
  qs <- rlang::enquos(...)
  attr(qs, "target_table") <- "tree"
  qs
}

#' Scoped Helper for Condition-Level Expressions
#'
#' Captures one or more expressions and tags them to be applied at the condition
#' table level during lazy evaluation. Used with `transform()`, `subset()`,
#' or `partition()` to explicitly scope mutations, filters, or domain
#' variables.
#'
#' @param ... Zero or more named or unnamed expressions.
#' @return A list of quosures tagged with `target_table = "cond"`.
#' @export
#' @examples
#' \dontrun{
#'   handler |>
#'     subset(cond(COND_STATUS_CD == 1)) |>
#'     partition(cond(FORTYPCD))
#'
#'   estimator |>
#'     estimate(cond())
#' }
cond <- function(...) {
  qs <- rlang::enquos(...)
  attr(qs, "target_table") <- "cond"
  qs
}

#' Scoped Helper for Plot-Level Expressions
#'
#' Captures one or more expressions and tags them to be applied at the plot
#' table level during lazy evaluation. Used with `transform()`, `subset()`,
#' or `partition()` to explicitly scope mutations, filters, or domain
#' variables.
#'
#' @param ... Zero or more named or unnamed expressions.
#' @return A list of quosures tagged with `target_table = "plot"`.
#' @export
#' @examples
#' \dontrun{
#'   handler |>
#'     subset(plot(STATECD == 50)) |>
#'     partition(plot(COUNTYCD))
#' }
plot <- function(...) {
  qs <- rlang::enquos(...)
  attr(qs, "target_table") <- "plot"
  qs
}

#' Scoped Helper for Previous-Plot-Level Expressions
#'
#' Captures one or more expressions and tags them to be applied at the previous
#' plot table level during lazy evaluation.
#'
#' @param ... Zero or more named or unnamed expressions.
#' @return A list of quosures tagged with `target_table = "pplot"`.
#' @export
pplot <- function(...) {
  qs <- rlang::enquos(...)
  attr(qs, "target_table") <- "pplot"
  qs
}

#' Scoped Helper for Previous-Condition-Level Expressions
#'
#' Captures one or more expressions and tags them to be applied at the previous
#' condition table level during lazy evaluation.
#'
#' @param ... Zero or more named or unnamed expressions.
#' @return A list of quosures tagged with `target_table = "pcond"`.
#' @export
pcond <- function(...) {
  qs <- rlang::enquos(...)
  attr(qs, "target_table") <- "pcond"
  qs
}

#' Scoped Helper for Previous-Tree-Level Expressions
#'
#' Captures one or more expressions and tags them to be applied at the previous
#' tree table level during lazy evaluation.
#'
#' @param ... Zero or more named or unnamed expressions.
#' @return A list of quosures tagged with `target_table = "ptree"`.
#' @export
ptree <- function(...) {
  qs <- rlang::enquos(...)
  attr(qs, "target_table") <- "ptree"
  qs
}

#' Scoped Helper for Tree-History-Level Expressions
#'
#' Captures one or more expressions and tags them to be applied at the
#' `tree_history` table level during lazy evaluation.
#'
#' @param ... Zero or more named or unnamed expressions.
#' @return A list of quosures tagged with `target_table = "tree_history"`.
#' @export
tree_history <- function(...) {
  qs <- rlang::enquos(...)
  attr(qs, "target_table") <- "tree_history"
  qs
}

#' Scoped Helper for Ratio Estimates
#'
#' Captures a numerator and denominator expression to specify a ratio
#' estimation intent.
#'
#' @param num A scoped target helper for the numerator, e.g., `tree(VOLCFNET)`
#' @param den A scoped target helper for the denominator, e.g., `cond()`
#' @return An object of class `fiaplyr_ratio_intent`.
#' @export
#' @examples
#' \dontrun{
#'   handler |> estimate(ratio(tree(VOLCFNET), cond()))
#' }
ratio <- function(num, den) {
  structure(
    list(numerator = num, denominator = den),
    class = "fiaplyr_ratio_intent"
  )
}