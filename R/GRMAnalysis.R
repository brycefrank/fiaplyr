#' GRM Analysis Spec
#'
#' @export
setClass(
  "GRMAnalysis",
  contains = "AnalysisSpec",
  slots = c(
    tree_basis = "character",
    land_basis = "character",
    component_rules = "list"
  ),
  prototype = list(
    tree_basis = "all_live",
    land_basis = "forest_land",
    component_rules = list()
  )
)

#' Create a GRM Analysis Specification
#'
#' Construct a [GRMAnalysis][GRMAnalysis-class] object for use with
#' [eval_handler()][eval_handler]. GRM analysis is a specification meant to
#' support the estimation of growth, removals, and mortality (GRM) attributes.
#' In contrast, [status_analysis()][status_analysis] is a specification meant
#' to support the estimation of the current status of, particularly, tree-
#' and condition-oriented attributes. Most GRM population parameters can be
#' estimated under this specification. Generally users should seek to employ
#' evaluations ending with `03`, indicating an evaluation engineered for GRM
#' analysis.
#'
#' Internally, this constructor validates the requested tree and land bases,
#' builds component rules for those bases, and stores them in a `GRMAnalysis` S4
#' object. When an evaluation handler uses the object, its methods build lazy
#' `dplyr` queries for current and prior plot, condition, and tree records,
#' along with GRM begin, midpoint, and component tables. These queries are
#' joined into a tree-history query, with required component columns selected
#' according to the configured tree basis.  Aggregation methods parse tree,
#' condition, or tree-history attributes and return lazy aggregate queries.
#'
#' @param tree_basis Tree basis preset. One of `all_live`,
#'   `growing_stock`, or `sawtimber`.
#' @param land_basis Land basis preset. One of `forest_land` or
#'   `timberland`.
#' @return A [GRMAnalysis][GRMAnalysis-class] object.
#' @export
#'
#' @examples
#' \dontrun{
#' handler <- eval_handler(con, 501103, spec = grm_analysis())
#' }
grm_analysis <- function(
  tree_basis = "all_live",
  land_basis = "forest_land"
) {
  tree_basis <- match.arg(
    tree_basis,
    c("all_live", "growing_stock", "sawtimber")
  )
  land_basis <- match.arg(land_basis, c("forest_land", "timberland"))

  rules <- build_grm_component_rules(tree_basis, land_basis)

  new(
    "GRMAnalysis",
    tree_basis = tree_basis,
    land_basis = land_basis,
    component_rules = rules
  )
}

#' Initialize Tables for GRM Analysis
#'
#' @param spec A GRMAnalysis object.
#' @param db A DBIConnection object.
#' @param evalid The evaluation ID.
#' @param backend Optional DatabaseMapping for custom schema/table names.
#' @return A list of lazy queries.
#' @noRd
setMethod(
  "initialize_tables",
  "GRMAnalysis",
  function(spec, db, evalid, backend = NULL) {
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

    pop_plot_stratum_assgn_qry <- dplyr::tbl(
      db,
      tbl_ref("POP_PLOT_STRATUM_ASSGN")
    ) %>%
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

    if (is.null(tree_grm_component_qry)) {
      stop(
        "TREE_GRM_COMPONENT is required for GRMAnalysis tree_history estimation.",
        call. = FALSE
      )
    }

    component_suffix <- .get_grm_component_suffix(spec)
    subptyp_col <- paste0("SUBP_SUBPTYP_GRM_", component_suffix)
    component_col <- paste0("SUBP_COMPONENT_", component_suffix)

    component_cols <- colnames(tree_grm_component_qry)
    missing_component_cols <- setdiff(
      c("TRE_CN", subptyp_col, component_col),
      component_cols
    )
    if (length(missing_component_cols) > 0) {
      stop(
        "TREE_GRM_COMPONENT is missing required column(s) for basis ",
        component_suffix,
        ": ",
        paste(missing_component_cols, collapse = ", "),
        call. = FALSE
      )
    }

    component_select <- list(
      CN = rlang::sym("TRE_CN"),
      SUBPTYP_GRM = rlang::sym(subptyp_col),
      COMPONENT = rlang::sym(component_col)
    )

    tree_grm_component_selected_qry <- tree_grm_component_qry %>%
      dplyr::transmute(!!!component_select)

    tree_history_qry <- tree_qry

    tree_history_qry <- tree_history_qry %>%
      dplyr::left_join(
        ptree_qry %>%
          dplyr::transmute(PREV_TRE_CN = CN, TPA_UNADJ_begin = TPA_UNADJ),
        by = "PREV_TRE_CN"
      )

    tree_history_qry <- tree_history_qry %>%
      dplyr::left_join(
        tree_grm_begin_qry,
        by = c("CN" = "TRE_CN"),
        suffix = c("", "_begin")
      )

    tree_history_qry <- tree_history_qry %>%
      dplyr::left_join(
        tree_grm_midpt_qry,
        by = c("CN" = "TRE_CN"),
        suffix = c("", "_midpt")
      )

    tree_history_qry <- tree_history_qry %>%
      dplyr::left_join(tree_grm_component_selected_qry, by = "CN")

    tree_history_qry <- tree_history_qry %>%
      dplyr::left_join(
        plot_qry %>%
          dplyr::transmute(PLT_CN = CN, PREV_PLT_CN),
        by = "PLT_CN"
      )

    if (!"PREVCOND" %in% colnames(tree_history_qry)) {
      tree_history_qry <- tree_history_qry %>%
        dplyr::left_join(
          ptree_qry %>% dplyr::transmute(PREV_TRE_CN = CN, PREVCOND = CONDID),
          by = "PREV_TRE_CN"
        )
    }

    cond_lookup_cols <- colnames(cond_qry)
    cond_lookup_transmute <- list(
      PLT_CN = rlang::sym("PLT_CN"),
      CONDID = rlang::sym("CONDID")
    )
    if ("COND_STATUS_CD" %in% cond_lookup_cols) {
      cond_lookup_transmute$COND_STATUS_CD <- rlang::sym("COND_STATUS_CD")
    }
    if ("SITECLCD" %in% cond_lookup_cols) {
      cond_lookup_transmute$SITECLCD <- rlang::sym("SITECLCD")
    }
    if ("RESERVCD" %in% cond_lookup_cols) {
      cond_lookup_transmute$RESERVCD <- rlang::sym("RESERVCD")
    }
    cond_lookup_qry <- cond_qry %>% dplyr::transmute(!!!cond_lookup_transmute)

    pcond_lookup_cols <- colnames(pcond_qry)
    pcond_lookup_transmute <- list(
      PREV_PLT_CN = rlang::sym("PLT_CN"),
      PREVCOND = rlang::sym("CONDID")
    )
    if ("COND_STATUS_CD" %in% pcond_lookup_cols) {
      pcond_lookup_transmute$PREV_COND_STATUS_CD <- rlang::sym("COND_STATUS_CD")
    }
    if ("SITECLCD" %in% pcond_lookup_cols) {
      pcond_lookup_transmute$PREV_SITECLCD <- rlang::sym("SITECLCD")
    }
    if ("RESERVCD" %in% pcond_lookup_cols) {
      pcond_lookup_transmute$PREV_RESERVCD <- rlang::sym("RESERVCD")
    }
    pcond_lookup_qry <- pcond_qry %>%
      dplyr::transmute(!!!pcond_lookup_transmute)

    tree_history_qry <- tree_history_qry %>%
      dplyr::left_join(cond_lookup_qry, by = c("PLT_CN", "CONDID")) %>%
      dplyr::left_join(pcond_lookup_qry, by = c("PREV_PLT_CN", "PREVCOND"))

    if (!"PREV_STATUS_CD" %in% colnames(tree_history_qry)) {
      if ("PREV_STATUSCD" %in% colnames(tree_history_qry)) {
        tree_history_qry <- tree_history_qry %>%
          dplyr::mutate(PREV_STATUS_CD = PREV_STATUSCD)
      } else if ("STATUSCD_begin" %in% colnames(tree_history_qry)) {
        tree_history_qry <- tree_history_qry %>%
          dplyr::mutate(PREV_STATUS_CD = STATUSCD_begin)
      }
    }

    required_cols <- c(
      "TREECLCD",
      "SPCD",
      "DIA",
      "DIA_begin",
      "STATUSCD",
      "AGENTCD",
      "PREV_STATUS_CD",
      "COND_STATUS_CD",
      "SITECLCD",
      "RESERVCD",
      "RECONCILECD",
      "PREV_COND_STATUS_CD",
      "PREV_SITECLCD",
      "PREV_RESERVCD"
    )
    missing_required <- setdiff(required_cols, colnames(tree_history_qry))
    if (length(missing_required) > 0) {
      tree_history_qry <- tree_history_qry %>%
        dplyr::mutate(
          !!!stats::setNames(
            rep(list(NA_real_), length(missing_required)),
            missing_required
          )
        )
    }

    component_rules <- spec@component_rules
    if (length(component_rules) == 0) {
      component_rules <- build_grm_component_rules(
        spec@tree_basis,
        spec@land_basis
      )
    }
    component_transition_expr <- .map_grm_component_transition_expr()

    tree_basis_filters <- get_tree_basis_filters(spec@tree_basis)
    land_basis_filters <- get_land_basis_filters(spec@land_basis)

    tree_history_qry <- tree_history_qry %>%
      dplyr::mutate(
        is_tree_basis_t1 = dplyr::coalesce(!!tree_basis_filters$t1, FALSE),
        is_tree_basis_t2 = dplyr::coalesce(!!tree_basis_filters$t2, FALSE),
        is_land_basis_t1 = dplyr::coalesce(!!land_basis_filters$t1, FALSE),
        is_land_basis_t2 = dplyr::coalesce(!!land_basis_filters$t2, FALSE),
        in_pop_t1 = is_tree_basis_t1 & is_land_basis_t1,
        in_pop_t2 = is_tree_basis_t2 & is_land_basis_t2,
        transition = !!component_transition_expr
      )

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
      tree_history = tree_history_qry
    )
  }
)

#' Aggregate Data for GRM Analysis
#'
#' @param spec A GRMAnalysis object.
#' @param handler The EvalHandler object.
#' @param ... Arguments for aggregation.
#' @return A lazy query with aggregates.
#' @noRd
setMethod("aggregate_data", "GRMAnalysis", function(spec, handler, ...) {
  args <- list(...)
  arg_names <- names(args)
  unnamed <- if (is.null(arg_names)) {
    rep(TRUE, length(args))
  } else {
    arg_names == ""
  }

  named_args <- if (is.null(arg_names)) {
    character(0)
  } else {
    arg_names[!unnamed & nzchar(arg_names)]
  }
  unknown_named <- setdiff(named_args, "sparse")
  if (length(unknown_named) > 0) {
    stop(
      "Unknown named argument(s) for `aggregate()`: ",
      paste(unknown_named, collapse = ", "),
      call. = FALSE
    )
  }

  if (!any(unnamed)) {
    stop(
      "Must provide exactly one scoped target helper such as `tree(VOLCFGRS)`, `cond()`, or `tree_history(...)`."
    )
  }

  if (sum(unnamed) != 1) {
    stop("`aggregate()` accepts exactly one scoped target helper per call.")
  }

  target_spec <- args[[which(unnamed)]]
  sparse <- if ("sparse" %in% names(args)) args[["sparse"]] else FALSE

  parsed <- .parse_target_spec(target_spec, "aggregate")
  slot_name <- parsed$slot
  targets <- parsed$targets
  target_names <- parsed$target_names
  target_quos <- parsed$quosures

  if (slot_name == "tree") {
    if (length(targets) == 0 || (length(targets) == 1 && targets == "1")) {
      return(.make_tree_aggregates(handler, sparse = sparse))
    }
    if (is.null(target_quos)) {
      target_quos <- rlang::syms(targets)
    }
    return(.make_tree_aggregates(handler, !!!target_quos, sparse = sparse))
  } else if (slot_name == "cond") {
    if (length(targets) > 0 && !all(targets == "1")) {
      stop(
        "Only `aggregate(cond())` or `aggregate(cond(1))` is currently supported for condition aggregation."
      )
    }
    res <- .make_cond_aggregates(handler, sparse = sparse)
    if (length(target_names) == 1 && nzchar(target_names[[1]])) {
      res <- res %>% dplyr::rename(!!target_names[[1]] := prop)
    }
    return(res)
  } else if (slot_name == "tree_history") {
    if (length(targets) == 0 || (length(targets) == 1 && targets == "1")) {
      return(.make_tree_history_aggregates(handler, sparse = sparse))
    }
    if (is.null(target_quos)) {
      target_quos <- rlang::syms(targets)
    }
    return(.make_tree_history_aggregates(
      handler,
      !!!target_quos,
      sparse = sparse
    ))
  } else {
    stop("Unsupported slot: ", slot_name)
  }
})

#' @describeIn spec_summary_fields GRMAnalysis-specific summary fields
#' @noRd
setMethod("spec_summary_fields", "GRMAnalysis", function(spec, handler) {
  list(
    tree_basis = spec@tree_basis,
    land_basis = spec@land_basis,
    n_component_rules = length(spec@component_rules)
  )
})

get_tree_basis_filters <- function(basis) {
  switch(
    basis,
    "all_live" = list(
      t1 = rlang::expr(DIA_begin >= 5.0),
      t2 = rlang::expr(DIA >= 5.0)
    ),
    "growing_stock" = list(
      t1 = rlang::expr(TREECLCD == 2 & DIA_begin >= 5.0),
      t2 = rlang::expr(TREECLCD == 2 & DIA >= 5.0)
    ),
    "sawtimber" = list(
      t1 = rlang::expr(
        TREECLCD == 2 &
          ((SPCD < 300 & DIA_begin >= 9.0) |
            (SPCD >= 300 & DIA_begin >= 11.0))
      ),
      t2 = rlang::expr(
        TREECLCD == 2 &
          ((SPCD < 300 & DIA >= 9.0) |
            (SPCD >= 300 & DIA >= 11.0))
      )
    ),
    stop(sprintf("Unknown tree basis: '%s'", basis))
  )
}

# internal function, not exported
get_land_basis_filters <- function(basis) {
  switch(
    basis,
    "forest_land" = list(
      t1 = rlang::expr(PREV_COND_STATUS_CD == 1),
      t2 = rlang::expr(COND_STATUS_CD == 1)
    ),
    "timberland" = list(
      t1 = rlang::expr(
        PREV_COND_STATUS_CD == 1 &
          PREV_SITECLCD %in% 1:6 &
          PREV_RESERVCD == 0
      ),
      t2 = rlang::expr(
        COND_STATUS_CD == 1 &
          SITECLCD %in% 1:6 &
          RESERVCD == 0
      )
    ),
    stop(sprintf("Unknown land basis: '%s'", basis))
  )
}

# internal function, not exported
build_grm_component_rules <- function(tree_basis, land_basis) {
  # Force basis validation where rules are constructed.
  get_tree_basis_filters(tree_basis)
  get_land_basis_filters(land_basis)

  status_live <- 1
  status_dead <- 2
  removal_agent_cd <- 80

  no_t2_status <- rlang::expr(
    is.na(STATUSCD) | !(STATUSCD %in% c(status_live, status_dead))
  )
  natural_mortality <- rlang::expr(is.na(AGENTCD) | AGENTCD != removal_agent_cd)
  harvest_removal <- rlang::expr(AGENTCD == removal_agent_cd)

  rules <- rlang::exprs(
    # Tree was live/dead at T1 but has no valid status at T2.
    PREV_STATUS_CD %in%
      c(status_live, status_dead) &
      !!no_t2_status ~ "not_used",

    # DIVERSION2: crossed the tree basis threshold, but land basis diverted by T2.
    !is_tree_basis_t1 &
      is_tree_basis_t2 &
      is_land_basis_t1 &
      !is_land_basis_t2 ~ "diversion",

    # DIVERSION1: was in-population at T1, but land basis diverted by T2.
    in_pop_t1 & is_land_basis_t1 & !is_land_basis_t2 ~ "diversion",

    in_pop_t1 &
      in_pop_t2 &
      PREV_STATUS_CD == status_live &
      STATUSCD == status_live ~ "survivor",

    in_pop_t1 &
      PREV_STATUS_CD == status_live &
      STATUSCD == status_dead &
      is_land_basis_t2 &
      !!natural_mortality ~ "mortality",

    in_pop_t1 &
      PREV_STATUS_CD == status_live &
      STATUSCD == status_dead &
      is_land_basis_t2 &
      !!harvest_removal ~ "removal",

    RECONCILECD == 1 & STATUSCD == status_live ~ "ingrowth",

    TRUE ~ "other"
  )

  rules
}

# internal function, not exported
.map_grm_component_transition_expr <- function() {
  component_norm <- rlang::expr(toupper(as.character(COMPONENT)))

  rlang::expr(
    dplyr::case_when(
      is.na(COMPONENT) ~ "unknown",
      !!component_norm == "SURVIVOR" ~ "survivor",
      !!component_norm == "DIVERSION0" ~ "diversion0",
      !!component_norm == "DIVERSION1" ~ "diversion1",
      !!component_norm == "DIVERSION2" ~ "diversion2",
      !!component_norm == "MORTALITY0" ~ "mortality0",
      !!component_norm == "MORTALITY1" ~ "mortality1",
      !!component_norm == "MORTALITY2" ~ "mortality2",
      !!component_norm == "CUT1" ~ "cut1",
      !!component_norm == "CUT2" ~ "cut2",
      !!component_norm == "INGROWTH" ~ "ingrowth",
      !!component_norm == "REVERSION1" ~ "reversion1",
      !!component_norm == "REVERSION2" ~ "reversion2",
      !!component_norm == "NOT USED" ~ "not_used",
      !!component_norm == "NOT_USED" ~ "not_used",
      !!component_norm == "NOTUSED" ~ "not_used",
      !!component_norm == "UNKNOWN" ~ "unknown",
      !!component_norm == "NA" ~ "na",
      !!component_norm == "N/A" ~ "na",
      !!component_norm == "N/A - A2A" ~ "na_a2a",
      !!component_norm == "N/A - A2A SOON" ~ "na_a2a_soon",
      !!component_norm == "N/A - MODELED" ~ "na_modeled",
      !!component_norm == "N/A - P2A" ~ "na_p2a",
      !!component_norm == "N/A - P2P" ~ "na_p2p",
      !!component_norm == "N/A - PERIODIC" ~ "na_periodic",
      !!component_norm == "CULLINCR" ~ "cullincr",
      !!component_norm == "CULLDECR" ~ "culldecr",
      TRUE ~ tolower(as.character(COMPONENT))
    )
  )
}

# internal function, not exported
.get_grm_component_suffix <- function(spec) {
  tree_code <- switch(
    spec@tree_basis,
    all_live = "AL",
    sawtimber = "SL",
    growing_stock = "GS",
    stop(sprintf("Unknown tree basis: '%s'", spec@tree_basis))
  )

  land_code <- switch(
    spec@land_basis,
    forest_land = "FOREST",
    timberland = "TIMBER",
    stop(sprintf("Unknown land basis: '%s'", spec@land_basis))
  )

  paste(tree_code, land_code, sep = "_")
}
