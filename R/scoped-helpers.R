#' Scope for Tree-Level Expressions
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

#' Scope for Condition-Level Expressions
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

#' Scope for Downed Woody Material Expressions
#'
#' Captures expressions to apply to the joined `COND_DWM_CALC` data during
#' [transform()], [subset()], [partition()], or [augment()]. Use the
#' component-specific `dwm_*()` helpers instead when selecting an aggregation
#' or estimation target.
#'
#' @param ... Zero or more named or unnamed expressions.
#' @return A list of quosures tagged with `target_table = "dwm"`.
#' @export
#' @examples
#' \dontrun{
#' handler |>
#'   transform(dwm(total_carbon = CWD_CARBON_ADJ + FWD_SM_CARBON_ADJ)) |>
#'   subset(dwm(total_carbon > 0)) |>
#'   partition(dwm(PHASE))
#' }
dwm <- function(...) {
  qs <- rlang::enquos(...)
  attr(qs, "target_table") <- "dwm"
  qs
}

#' Scope for Plot-Level Expressions
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

#' Scope for Previous-Plot-Level Expressions
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

#' Scope for Previous-Condition-Level Expressions
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

#' Scope for Previous-Tree-Level Expressions
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

#' Scope for Tree-History-Level Expressions
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

#' Scope for Ratio Estimates
#'
#' Captures a numerator and denominator expression to specify a ratio
#' estimation intent.
#'
#' @param num A scope for the numerator, e.g., `tree(VOLCFNET)`
#' @param den A scope for the denominator, e.g., `cond()`
#' @param den_partitions Optional denominator-only domain overrides expressed as
#'   scopes, either as a single scope (for example `cond(FORTYPCD)`) or
#'   a list of scopes (for example `list(cond(FORTYPCD), tree(SPCD))`).
#' @return An object of class `fiaplyr_ratio_intent`.
#' @export
#' @examples
#' \dontrun{
#'   handler |> estimate(ratio(tree(VOLCFNET), cond()))
#'   handler |> estimate(ratio(tree(VOLCFNET), cond(), den_partitions = list(cond(FORTYPCD))))
#' }
ratio <- function(num, den, den_partitions = NULL) {
  structure(
    list(numerator = num, denominator = den, den_partitions = den_partitions),
    class = "fiaplyr_ratio_intent"
  )
}