#' Parse Estimation Formula
#'
#' Parses a formula of the form `slot ~ variable | variable2` into its
#' components.
#'
#' @param f A formula.
#' @return A list containing the slot name and a character vector of target
#'  variables.
#' @noRd
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
    targets = targets,
    target_names = rep("", length(targets)),
    quosures = NULL
  )
}

.parse_target_helper <- function(spec) {
  target_table <- attr(spec, "target_table")

  if (is.null(target_table) || !target_table %in% c("tree", "cond", "plot", "dwm", "tree_history")) {
    stop("Scoped target helpers must use `tree()`, `cond()`, `plot()`, `dwm()`, or `tree_history()`.")
  }

  if (target_table == "dwm") {
    return(.parse_dwm_scoped_spec(spec))
  }

  targets <- vapply(spec, rlang::as_label, character(1))
  target_names <- names(spec)
  if (is.null(target_names)) {
    target_names <- rep("", length(targets))
  }
  target_names[is.na(target_names)] <- ""

  list(
    slot = target_table,
    targets = targets,
    target_names = target_names,
    quosures = spec,
    dwm_targets = NULL
  )
}

.parse_dwm_scoped_spec <- function(spec) {
  if (length(spec) == 0) {
    stop(
      "`dwm()` requires a DWM component helper such as `dwm_cwd(CARBON)`, e.g. `dwm(dwm_cwd(CARBON))`.",
      call. = FALSE
    )
  }

  exprs <- lapply(spec, rlang::quo_get_expr)
  is_component <- vapply(
    exprs,
    function(e) rlang::is_call(e) && rlang::call_name(e) %in% .dwm_component_helpers,
    logical(1)
  )
  if (!all(is_component)) {
    stop(
      "`dwm()` must contain only DWM component helpers such as `dwm_cwd(CARBON)`, not raw expressions.",
      call. = FALSE
    )
  }

  helpers <- lapply(spec, rlang::eval_tidy)
  if (!all(vapply(helpers, inherits, logical(1), what = "dwm_target"))) {
    stop(
      "`dwm()` must contain only DWM component helpers such as `dwm_cwd(CARBON)`, not raw expressions.",
      call. = FALSE
    )
  }

  supplied_names <- names(spec)
  if (is.null(supplied_names)) {
    supplied_names <- rep("", length(spec))
  }

  dwm_targets <- lapply(seq_along(helpers), function(i) {
    target <- helpers[[i]]
    if (nzchar(supplied_names[[i]])) {
      target$name <- supplied_names[[i]]
    }
    target
  })
  output_names <- unname(vapply(dwm_targets, function(target) target$name, character(1)))

  list(
    slot = "dwm",
    targets = output_names,
    target_names = rep("", length(output_names)),
    quosures = NULL,
    dwm_targets = dwm_targets
  )
}

.parse_target_spec <- function(spec, caller) {
  if (inherits(spec, "formula")) {
    parsed <- parse_formula(spec)
    replacement <- if (parsed$slot == "cond" && identical(parsed$targets, "1")) {
      paste0(caller, "(cond())")
    } else {
      paste0(caller, "(", parsed$slot, "(", paste(parsed$targets, collapse = ", "), "))")
    }

    lifecycle::deprecate_warn(
      "0.1.0",
      what = paste0(caller, "() with formula targets"),
      details = paste0("Use `", replacement, "` instead.")
    )

    return(parsed)
  }

  if (inherits(spec, "fiaplyr_target")) {
    if (identical(spec$scope, "dwm")) {
      stop(
        "DWM component targets must be wrapped in `dwm()`, e.g. `dwm(dwm_cwd(CARBON))`.",
        call. = FALSE
      )
    }
    stop(
      "Target helpers must be wrapped in the scoped helper matching their scope, e.g. `",
      spec$scope,
      "(...)`.",
      call. = FALSE
    )
  }

  if (is.list(spec) && !is.null(attr(spec, "target_table"))) {
    return(.parse_target_helper(spec))
  }

  stop(
    "Must provide exactly one scoped target helper such as `tree(VOLCFGRS)` or `cond()`."
  )
}
