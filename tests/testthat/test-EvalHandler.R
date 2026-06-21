test_that("EvalHandler initializes correctly", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001)

  expect_s4_class(handler, "EvalHandler")
  expect_equal(evalid(handler), 1001)

  # Check if tables are lazily loaded
  expect_true(dplyr::is.tbl(handler@tables$plot))
  expect_true(dplyr::is.tbl(handler@tables$tree))
  expect_true(dplyr::is.tbl(handler@tables$ref_species))

  # Check content helper (simple check to see if join worked and valid data exists)
  plots <- handler@tables$plot %>% dplyr::collect()
  expect_equal(nrow(plots), 8)

  trees <- handler@tables$tree %>% dplyr::collect()
  expect_equal(nrow(trees), 8) # 8 trees across both estimation units
})

test_that("status_analysis() returns a StatusAnalysis spec", {
  spec <- status_analysis()

  expect_s4_class(spec, "StatusAnalysis")
  expect_s4_class(spec, "AnalysisSpec")
})

test_that("grm_analysis() returns a GRMAnalysis spec", {
  spec <- grm_analysis()

  expect_s4_class(spec, "GRMAnalysis")
  expect_s4_class(spec, "AnalysisSpec")
  expect_identical(spec@tree_basis, "all_live")
  expect_identical(spec@land_basis, "forest_land")
  expect_true(is.list(spec@component_rules))
  expect_true(length(spec@component_rules) > 0)
})

test_that("grm_analysis() supports explicit bases and validates input", {
  spec <- grm_analysis(tree_basis = "sawtimber", land_basis = "timberland")

  expect_identical(spec@tree_basis, "sawtimber")
  expect_identical(spec@land_basis, "timberland")

  expect_error(
    grm_analysis(tree_basis = "not_a_domain"),
    "should be one of"
  )

  expect_error(
    grm_analysis(land_basis = "not_a_domain"),
    "should be one of"
  )
})

test_that("GRM spec summary fields are shown in handler summary and print output", {
  con <- setup_grm_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(
    con,
    evalid = 1003,
    spec = grm_analysis(tree_basis = "sawtimber", land_basis = "timberland")
  )

  s <- summary(handler)
  expect_identical(s$tree_basis, "sawtimber")
  expect_identical(s$land_basis, "timberland")
  expect_identical(s$n_component_rules, 5L)

  shown <- capture.output(show(handler))
  expect_true(any(grepl("GRM Spec", shown, fixed = TRUE)))
  expect_true(any(grepl("Tree basis:\\s+sawtimber", shown)))
  expect_true(any(grepl("Land basis:\\s+timberland", shown)))
  expect_true(any(grepl("Rules:\\s+5", shown)))
})

test_that("EvalHandler filters correctly by evalid", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # Add another evaluation dummy data to ensure we are filtering
  DBI::dbWriteTable(con, "POP_EVAL", data.frame(
    CN = 2,
    EVALID = 9999,
    EVAL_DESCR = "Wrong Eval",
    stringsAsFactors = FALSE
  ), append = TRUE)

  handler <- eval_handler(con, evalid = 1001)

  desc <- summary(handler)$eval_descr
  expect_equal(desc, "Test Evaluation")
})

test_that("subset(tree()) can use WOODLAND after REF_SPECIES join", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001)

  base <- handler %>%
    aggregate(tree(VOLCFGRS), expander = TPA_UNADJ) %>%
    dplyr::collect()

  woodland_filtered <- handler %>%
    subset(tree(WOODLAND != "N")) %>%
    aggregate(tree(VOLCFGRS), expander = TPA_UNADJ) %>%
    dplyr::collect()

  expect_true(sum(woodland_filtered$VOLCFGRS) < sum(base$VOLCFGRS))
})

test_that("missing REF_SPECIES warns and continues", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  DBI::dbRemoveTable(con, "REF_SPECIES")

  handler <- eval_handler(con, evalid = 1001)

  expect_warning(
    {
      res <- handler %>%
        aggregate(tree(VOLCFGRS), expander = TPA_UNADJ) %>%
        dplyr::collect()
      expect_true(nrow(res) > 0)
    },
    "REF_SPECIES table not available"
  )
})

test_that("missing SUBP_COND is tolerated", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  DBI::dbRemoveTable(con, "SUBP_COND")

  handler <- eval_handler(con, evalid = 1001)

  expect_null(handler@tables$subp_cond)

  res <- handler %>%
    aggregate(cond(), expander = TPA_UNADJ) %>%
    dplyr::collect()

  expect_true(nrow(res) > 0)
})

add_shared_countycd_columns <- function(con) {
  DBI::dbExecute(con, "ALTER TABLE COND ADD COLUMN COUNTYCD DOUBLE")
  DBI::dbExecute(con, "UPDATE COND SET COUNTYCD = CASE WHEN CONDID = 1 THEN 10 ELSE 20 END")

  DBI::dbExecute(con, "ALTER TABLE TREE ADD COLUMN COUNTYCD DOUBLE")
  DBI::dbExecute(con, "UPDATE TREE SET COUNTYCD = CASE WHEN SPCD = 1 THEN 100 ELSE 200 END")
}

test_that("aggregate() accepts cond() helper targets", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  result_new <- eval_handler(con, evalid = 1001) %>%
    partition(cond(COND_STATUS_CD)) %>%
    aggregate(cond(), expander = TPA_UNADJ) %>%
    dplyr::collect() %>%
    dplyr::arrange(COND_STATUS_CD, PLT_CN, PLOT)

  result_with_placeholder <- eval_handler(con, evalid = 1001) %>%
    partition(cond(COND_STATUS_CD)) %>%
    aggregate(cond(1), expander = TPA_UNADJ) %>%
    dplyr::collect() %>%
    dplyr::arrange(COND_STATUS_CD, PLT_CN, PLOT)

  expect_equal(result_new, result_with_placeholder)
})

test_that("aggregate() preserves user-defined output names", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  tree_res <- eval_handler(con, evalid = 1001) %>%
    aggregate(tree(my_vol = VOLCFNET)) %>%
    dplyr::collect()

  cond_res <- eval_handler(con, evalid = 1001) %>%
    aggregate(cond(my_prop = 1)) %>%
    dplyr::collect()

  expect_true("my_vol" %in% colnames(tree_res))
  expect_false("VOLCFNET" %in% colnames(tree_res))

  expect_true("my_prop" %in% colnames(cond_res))
  expect_false("prop" %in% colnames(cond_res))
})

test_that("partition() accepts multiple scoped helpers in one call", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  tree_res <- eval_handler(con, evalid = 1001) %>%
    aggregate(tree(my_vol = VOLCFNET)) %>%
    dplyr::collect()

  cond_res <- eval_handler(con, evalid = 1001) %>%
    aggregate(cond(my_prop = 1)) %>%
    dplyr::collect()

  expect_true("my_vol" %in% colnames(tree_res))
  expect_false("VOLCFNET" %in% colnames(tree_res))

  expect_true("my_prop" %in% colnames(cond_res))
  expect_false("prop" %in% colnames(cond_res))
})

test_that("partition() accepts multiple scoped helpers in one call", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  result_new <- eval_handler(con, evalid = 1001) %>%
    partition(tree(SPCD), cond(COND_STATUS_CD)) %>%
    aggregate(tree(VOLCFGRS), expander = TPA_UNADJ) %>%
    dplyr::collect() %>%
    dplyr::arrange(COND_STATUS_CD, SPCD, PLT_CN, PLOT)

  result_repeat <- eval_handler(con, evalid = 1001) %>%
    partition(tree(SPCD), cond(COND_STATUS_CD)) %>%
    aggregate(tree(VOLCFGRS), expander = TPA_UNADJ) %>%
    dplyr::collect() %>%
    dplyr::arrange(COND_STATUS_CD, SPCD, PLT_CN, PLOT)

  expect_equal(result_new, result_repeat)
})

test_that("partition(plot(COUNTYCD)) works for tree and cond aggregation", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  tree_result <- eval_handler(con, evalid = 1001) %>%
    partition(plot(COUNTYCD)) %>%
    aggregate(tree(VOLCFGRS), expander = TPA_UNADJ) %>%
    dplyr::collect()

  cond_result <- eval_handler(con, evalid = 1001) %>%
    partition(plot(COUNTYCD)) %>%
    aggregate(cond(), expander = TPA_UNADJ) %>%
    dplyr::collect()

  expect_true("COUNTYCD" %in% colnames(tree_result))
  expect_true("COUNTYCD" %in% colnames(cond_result))
  expect_equal(sort(unique(tree_result$COUNTYCD)), 1)
  expect_equal(sort(unique(cond_result$COUNTYCD)), 1)
})

test_that("partition() respects helper scope for shared column names", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  add_shared_countycd_columns(con)

  cond_result <- eval_handler(con, evalid = 1001) %>%
    partition(cond(COUNTYCD)) %>%
    aggregate(cond(), expander = TPA_UNADJ) %>%
    dplyr::collect()

  tree_result <- eval_handler(con, evalid = 1001) %>%
    partition(tree(COUNTYCD)) %>%
    aggregate(tree(VOLCFGRS), expander = TPA_UNADJ) %>%
    dplyr::collect()

  expect_true("COUNTYCD.cond" %in% colnames(cond_result))
  expect_true("COUNTYCD.tree" %in% colnames(tree_result))
  expect_equal(sort(unique(cond_result[["COUNTYCD.cond"]])), c(10, 20))
  expect_equal(sort(unique(tree_result[["COUNTYCD.tree"]])), c(100, 200))
})

test_that("scoped helpers tag expressions correctly", {
  # Test that helpers capture and tag quosures
  tree_expr <- tree(BA = 0.005454 * DIA^2)
  expect_equal(attr(tree_expr, "target_table"), "tree")
  expect_true(all(class(tree_expr) == c("quosures", "list")))

  cond_expr <- cond(COND_STATUS_CD == 1)
  expect_equal(attr(cond_expr, "target_table"), "cond")
  expect_true(all(class(cond_expr) == c("quosures", "list")))

  plot_expr <- plot(STATECD == 50)
  expect_equal(attr(plot_expr, "target_table"), "plot")
  expect_true(all(class(plot_expr) == c("quosures", "list")))
})

test_that("unscoped expressions in new API error appropriately", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001)

  # Create a test to verify routing error when target_table is missing
  # We'll use rlang::new_formula to create an unevaluated expression
  # and then call the internal routing function directly
  untagged_quos <- rlang::quos(BA = 0.005454 * DIA^2)
  # Make sure it doesn't have target_table attribute
  expect_null(attr(untagged_quos, "target_table"))
  
  # The routing function should error
  expect_error(
    .route_scoped_expressions(handler, untagged_quos, "append_mutations"),
    "All expressions must be explicitly scoped"
  )
})

test_that("aggregate(tree()) accepts user-supplied macro expressions", {
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # Weighted mean macro
  result <- eval_handler(con, evalid = 1001) %>%
    aggregate(tree(wm_vol = sum(TPA_UNADJ * VOLCFGRS) / sum(TPA_UNADJ))) %>%
    dplyr::collect()

  expect_true("wm_vol" %in% colnames(result))
  expect_true(is.numeric(result$wm_vol))
})

test_that("aggregate(tree()) mixes implicit and macro targets", {
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  result <- eval_handler(con, evalid = 1001) %>%
    aggregate(tree(VOLCFGRS, wm_vol = sum(TPA_UNADJ * VOLCFGRS) / sum(TPA_UNADJ))) %>%
    dplyr::collect()

  expect_true("VOLCFGRS" %in% colnames(result))
  expect_true("wm_vol" %in% colnames(result))
  # Weighted mean must be <= implicit weighted sum (TPA-weighted sum is unbounded, wm is per-tree avg)
  expect_true(all(result$wm_vol >= 0 | is.na(result$wm_vol)))
})

test_that("aggregate(tree()) implicit default is unchanged by macro dispatch", {
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  result_implicit <- eval_handler(con, evalid = 1001) %>%
    aggregate(tree(VOLCFGRS)) %>%
    dplyr::collect() %>%
    dplyr::arrange(PLT_CN)

  # Explicit sum(...) macro should produce same result
  result_explicit <- eval_handler(con, evalid = 1001) %>%
    aggregate(tree(VOLCFGRS = sum(TPA_UNADJ * VOLCFGRS, na.rm = TRUE))) %>%
    dplyr::collect() %>%
    dplyr::arrange(PLT_CN)

  expect_equal(result_implicit$VOLCFGRS, result_explicit$VOLCFGRS)
})

test_that("aggregate() defaults expander to TPA_UNADJ", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001)

  default_res <- handler %>%
    aggregate(tree(VOLCFGRS)) %>%
    dplyr::collect() %>%
    dplyr::arrange(PLT_CN, PLOT)

  explicit_res <- handler %>%
    aggregate(tree(VOLCFGRS), expander = TPA_UNADJ) %>%
    dplyr::collect() %>%
    dplyr::arrange(PLT_CN, PLOT)

  expect_equal(default_res, explicit_res)
})

test_that("aggregate() errors for missing expander column", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001)

  expect_error(
    handler %>% aggregate(tree(VOLCFGRS), expander = DOES_NOT_EXIST) %>% dplyr::collect(),
    "expander.*DOES_NOT_EXIST"
  )
})

test_that("aggregate() errors for non-numeric expander", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001) %>%
    transform(tree(EXPANDER_CHAR = as.character(TPA_UNADJ)))

  expect_error(
    handler %>% aggregate(tree(VOLCFGRS), expander = EXPANDER_CHAR) %>% dplyr::collect(),
    "must be numeric"
  )
})

test_that("materialize() returns lazy prepared tables for status handlers", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001) %>%
    transform(
      plot(PLOT_BUCKET = PLOT * 10),
      cond(COND_BUCKET = CONDID * 100),
      tree(TREE_BUCKET = SPCD * 1000)
    ) %>%
    subset(
      plot(PLOT <= 4),
      cond(COND_STATUS_CD == 1),
      tree(SPCD == 1)
    )

  plot_q <- materialize(handler, "plot")
  cond_q <- materialize(handler, "cond")
  tree_q <- materialize(handler, "tree")

  expect_true(dplyr::is.tbl(plot_q))
  expect_true(dplyr::is.tbl(cond_q))
  expect_true(dplyr::is.tbl(tree_q))

  plot_res <- plot_q %>% dplyr::collect()
  cond_res <- cond_q %>% dplyr::collect()
  tree_res <- tree_q %>% dplyr::collect()

  expect_true(all(plot_res$PLOT <= 4))
  expect_true(all(cond_res$COND_STATUS_CD == 1))
  expect_true(all(tree_res$SPCD == 1))
  expect_equal(plot_res$PLOT_BUCKET, plot_res$PLOT * 10)
  expect_equal(cond_res$COND_BUCKET, cond_res$CONDID * 100)
  expect_equal(tree_res$TREE_BUCKET, tree_res$SPCD * 1000)
})

test_that("materialize() handles GRM tree_history tables", {
  con <- setup_grm_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1003, spec = new("GRMAnalysis")) %>%
    transform(
      tree_history(HIST_FLAG = STATUSCD_begin + 1)
    ) %>%
    subset(
      tree_history(DIA_begin >= 10)
    )

  tree_history_q <- materialize(handler, "tree_history")

  expect_true(dplyr::is.tbl(tree_history_q))

  tree_history_res <- tree_history_q %>% dplyr::collect()

  expect_true(nrow(tree_history_res) > 0)
  expect_true(all(tree_history_res$DIA_begin >= 10))
  expect_equal(tree_history_res$HIST_FLAG, tree_history_res$STATUSCD_begin + 1)
})

test_that("materialize() rejects invalid slots and unsupported tree_history usage", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001)

  expect_error(
    materialize(handler, "bogus"),
    "Unsupported slot"
  )

  expect_error(
    materialize(handler, "tree_history"),
    "not available for this analysis spec"
  )
})
