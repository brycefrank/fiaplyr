#' GRM Analysis Spec
#'
#' @export
setClass("GRMAnalysis", contains = "AnalysisSpec")

#' Initialize Tables for GRM Analysis
#'
#' @param spec A GRMAnalysis object.
#' @param db A DBIConnection object.
#' @param evalid The evaluation ID.
#' @param backend Optional DatabaseMapping for custom schema/table names.
#' @return A list of lazy queries.
#' @export
setMethod("initialize_tables", "GRMAnalysis", function(spec, db, evalid, backend = NULL) {
  if (is.null(backend)) {
    backend <- database_mapping()
  }

  tbl_ref <- function(name) get_table_ref(backend, name)

  pop_eval_qry <- dplyr::tbl(db, tbl_ref("POP_EVAL")) %>%
    dplyr::filter(EVALID == !!evalid)

  pop_estn_unit_qry <- dplyr::tbl(db, tbl_ref("POP_ESTN_UNIT")) %>%
    dplyr::semi_join(pop_eval_qry, by = c("EVAL_CN" = "CN"))

  pop_stratum_qry <- dplyr::tbl(db, tbl_ref("POP_STRATUM")) %>%
    dplyr::semi_join(pop_estn_unit_qry, by = c("ESTN_UNIT_CN" = "CN"))

  pop_plot_stratum_assgn_qry <- dplyr::tbl(db, tbl_ref("POP_PLOT_STRATUM_ASSGN")) %>%
    dplyr::semi_join(pop_stratum_qry, by = c("STRATUM_CN" = "CN"))

  plot_qry <- dplyr::tbl(db, tbl_ref("PLOT")) %>%
    dplyr::semi_join(pop_plot_stratum_assgn_qry, by = c("CN" = "PLT_CN"))

  pplot_qry <- dplyr::tbl(db, tbl_ref("PLOT")) %>%
    dplyr::semi_join(
      plot_qry %>%
        dplyr::filter(!is.na(PREV_PLT_CN)) %>%
        dplyr::transmute(CN = PREV_PLT_CN) %>%
        dplyr::distinct(),
      by = "CN"
    )

  cond_qry <- dplyr::tbl(db, tbl_ref("COND")) %>%
    dplyr::semi_join(plot_qry, by = c("PLT_CN" = "CN"))

  pcond_qry <- dplyr::tbl(db, tbl_ref("COND")) %>%
    dplyr::semi_join(pplot_qry, by = c("PLT_CN" = "CN"))

  tree_qry <- dplyr::tbl(db, tbl_ref("TREE")) %>%
    dplyr::semi_join(cond_qry, by = c("PLT_CN", "CONDID"))

  ptree_qry <- dplyr::tbl(db, tbl_ref("TREE")) %>%
    dplyr::semi_join(
      tree_qry %>%
        dplyr::filter(!is.na(PREV_TRE_CN)) %>%
        dplyr::transmute(CN = PREV_TRE_CN) %>%
        dplyr::distinct(),
      by = "CN"
    )

  ref_species_qry <- tryCatch(
    dplyr::tbl(db, tbl_ref("REF_SPECIES")),
    error = function(e) NULL
  )

  subp_cond_qry <- tryCatch(
    dplyr::tbl(db, tbl_ref("SUBP_COND")) %>%
      dplyr::semi_join(cond_qry, by = c("PLT_CN", "CONDID")),
    error = function(e) NULL
  )

  tree_grm_begin_qry <- tryCatch(
    dplyr::tbl(db, tbl_ref("TREE_GRM_BEGIN")) %>%
      dplyr::semi_join(tree_qry, by = c("TRE_CN" = "CN")),
    error = function(e) NULL
  )

  tree_grm_midpt_qry <- tryCatch(
    dplyr::tbl(db, tbl_ref("TREE_GRM_MIDPT")) %>%
      dplyr::semi_join(tree_qry, by = c("TRE_CN" = "CN")),
    error = function(e) NULL
  )

  tree_grm_component_qry <- tryCatch(
    dplyr::tbl(db, tbl_ref("TREE_GRM_COMPONENT")) %>%
      dplyr::semi_join(tree_qry, by = c("TRE_CN" = "CN")),
    error = function(e) NULL
  )

  beginend_qry <- tryCatch(
    dplyr::tbl(db, tbl_ref("BEGINEND")),
    error = function(e) NULL
  )

  tree_history_qry <- tree_qry

  tree_history_qry <- tree_history_qry %>%
    dplyr::left_join(
      ptree_qry %>%
        dplyr::transmute(PREV_TRE_CN = CN, TPA_UNADJ_begin = TPA_UNADJ),
      by = "PREV_TRE_CN"
    )

  tree_history_qry <- tree_history_qry %>%
    dplyr::left_join(tree_grm_begin_qry, by = c("CN" = "TRE_CN"), suffix = c("", "_begin"))

  tree_history_qry <- tree_history_qry %>%
    dplyr::left_join(tree_grm_midpt_qry, by = c("CN" = "TRE_CN"), suffix = c("", "_midpt"))

  tree_history_qry <- tree_history_qry %>%
    dplyr::left_join(tree_grm_component_qry, by = c("CN" = "TRE_CN"), suffix = c("", "_component"))

  list(
    pop_eval = pop_eval_qry,
    pop_estn_unit = pop_estn_unit_qry,
    pop_stratum = pop_stratum_qry,
    pop_plot_stratum_assgn = pop_plot_stratum_assgn_qry,
    plot = plot_qry,
    pplot = pplot_qry,
    cond = cond_qry,
    pcond = pcond_qry,
    tree = tree_qry,
    ptree = ptree_qry,
    ref_species = ref_species_qry,
    subp_cond = subp_cond_qry,
    tree_grm_begin = tree_grm_begin_qry,
    tree_grm_midpt = tree_grm_midpt_qry,
    tree_grm_component = tree_grm_component_qry,
    tree_history = tree_history_qry,
    beginend = beginend_qry
  )
})

#' Aggregate Data for GRM Analysis
#'
#' @param spec A GRMAnalysis object.
#' @param handler The EvalHandler object.
#' @param ... Arguments for aggregation.
#' @param expander Tree expansion column used when aggregating tree-level
#'   summaries (for example, `TPA_UNADJ`).
#' @return A lazy query with aggregates.
#' @export
setMethod("aggregate_data", "GRMAnalysis", function(spec, handler, ..., expander) {
  args <- list(...)
  arg_names <- names(args)
  unnamed <- if (is.null(arg_names)) rep(TRUE, length(args)) else arg_names == ""

  named_args <- if (is.null(arg_names)) character(0) else arg_names[!unnamed & nzchar(arg_names)]
  unknown_named <- setdiff(named_args, "sparse")
  if (length(unknown_named) > 0) {
    stop("Unknown named argument(s) for `aggregate()`: ", paste(unknown_named, collapse = ", "), call. = FALSE)
  }

  if (!any(unnamed)) {
    stop("Must provide exactly one scoped target helper such as `tree(VOLCFGRS)`, `cond()`, or `tree_history(...)`.")
  }

  if (sum(unnamed) != 1) {
    stop("`aggregate()` accepts exactly one scoped target helper per call.")
  }

  target_spec <- args[[which(unnamed)]]
  sparse <- if ("sparse" %in% names(args)) args[["sparse"]] else FALSE
  expander_name <- rlang::as_name(rlang::ensym(expander))

  parsed <- .parse_target_spec(target_spec, "aggregate")
  slot_name <- parsed$slot
  targets <- parsed$targets

  if (slot_name == "tree") {
    if (length(targets) == 0 || (length(targets) == 1 && targets == "1")) {
      return(.make_tree_aggregates(handler, sparse = sparse, expander = expander_name))
    }
    syms <- rlang::syms(targets)
    return(.make_tree_aggregates(handler, !!!syms, sparse = sparse, expander = expander_name))
  } else if (slot_name == "cond") {
    if (length(targets) > 0 && !all(targets == "1")) {
      stop("Only `aggregate(cond())` or `aggregate(cond(1))` is currently supported for condition aggregation.")
    }
    return(.make_cond_aggregates(handler, sparse = sparse))
  } else if (slot_name == "tree_history") {
    if (length(targets) == 0 || (length(targets) == 1 && targets == "1")) {
      return(.make_tree_history_aggregates(handler, sparse = sparse, expander = expander_name))
    }
    syms <- rlang::syms(targets)
    return(.make_tree_history_aggregates(handler, !!!syms, sparse = sparse, expander = expander_name))
  } else {
    stop("Unsupported slot: ", slot_name)
  }
})

#' @describeIn spec_summary_fields GRMAnalysis-specific summary fields
#' @export
setMethod("spec_summary_fields", "GRMAnalysis", function(spec, handler) {
  # Placeholder for GRM-specific summary fields.
  list()
})