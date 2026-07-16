setClass("Selector", contains = "VIRTUAL")

setClass(
  "EvalSelector",
  contains = "Selector",
  slots = list(evalid = "numeric")
)

setClass(
  "WindowSelector",
  contains = "Selector",
  slots = list(window = "ANY")
)

.as_selector <- function(selector) {
  if (methods::is(selector, "Selector")) {
    return(selector)
  }

  if (inherits(selector, "fiaplyr_window")) {
    return(new("WindowSelector", window = selector))
  }

  if (is.numeric(selector) && length(selector) == 1 && !is.na(selector)) {
    return(new("EvalSelector", evalid = selector))
  }

  stop(
    "`selector` must be a single numeric EVALID, a Selector object, or a fiaplyr window specification.",
    call. = FALSE
  )
}

setGeneric("resolve_selection_tables", function(selector, db, backend = NULL) {
  standardGeneric("resolve_selection_tables")
})

setMethod(
  "resolve_selection_tables",
  signature = c(selector = "EvalSelector"),
  definition = function(selector, db, backend = NULL) {
    if (is.null(backend)) {
      backend <- database_mapping()
    }

    tbl_ref <- function(name) get_table_ref(backend, name)

    pop_eval_qry <- dplyr::tbl(db, tbl_ref("POP_EVAL")) %>%
      dplyr::filter(EVALID == !!selector@evalid)

    pop_estn_unit_qry <- dplyr::tbl(db, tbl_ref("POP_ESTN_UNIT")) %>%
      dplyr::semi_join(pop_eval_qry, by = c("EVAL_CN" = "CN"))

    pop_stratum_qry <- dplyr::tbl(db, tbl_ref("POP_STRATUM")) %>%
      dplyr::semi_join(pop_estn_unit_qry, by = c("ESTN_UNIT_CN" = "CN"))

    pop_plot_stratum_assgn_qry <- dplyr::tbl(db, tbl_ref("POP_PLOT_STRATUM_ASSGN")) %>%
      dplyr::semi_join(pop_stratum_qry, by = c("STRATUM_CN" = "CN"))

    plot_qry <- dplyr::tbl(db, tbl_ref("PLOT")) %>%
      dplyr::semi_join(pop_plot_stratum_assgn_qry, by = c("CN" = "PLT_CN"))

    list(
      pop_eval = pop_eval_qry,
      pop_estn_unit = pop_estn_unit_qry,
      pop_stratum = pop_stratum_qry,
      pop_plot_stratum_assgn = pop_plot_stratum_assgn_qry,
      plot = plot_qry
    )
  }
)

setMethod(
  "resolve_selection_tables",
  signature = c(selector = "WindowSelector"),
  definition = function(selector, db, backend = NULL) {
    list(
      pop_eval = NULL,
      pop_estn_unit = NULL,
      pop_stratum = NULL,
      pop_plot_stratum_assgn = NULL,
      plot = .window_plot_query(db, selector@window, backend)
    )
  }
)
