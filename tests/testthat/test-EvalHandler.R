test_that("EvalHandler initializes correctly", {
  con <- setup_test_db()
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

test_that("EvalHandler filters correctly by evalid", {
  con <- setup_test_db()
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

test_that("filter_tree can use WOODLAND after REF_SPECIES join", {
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001)

  base <- handler %>%
    aggregate(tree(VOLCFGRS)) %>%
    dplyr::collect()

  woodland_filtered <- handler %>%
    filter_tree(WOODLAND != "N") %>%
    aggregate(tree(VOLCFGRS)) %>%
    dplyr::collect()

  expect_true(sum(woodland_filtered$VOLCFGRS) < sum(base$VOLCFGRS))
})

test_that("missing REF_SPECIES warns and continues", {
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  DBI::dbRemoveTable(con, "REF_SPECIES")

  handler <- eval_handler(con, evalid = 1001)

  expect_warning(
    {
      res <- handler %>%
        aggregate(tree(VOLCFGRS)) %>%
        dplyr::collect()
      expect_true(nrow(res) > 0)
    },
    "REF_SPECIES table not available"
  )
})

test_that("missing SUBP_COND is tolerated", {
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  DBI::dbRemoveTable(con, "SUBP_COND")

  handler <- eval_handler(con, evalid = 1001)

  expect_null(handler@tables$subp_cond)

  res <- handler %>%
    aggregate(cond()) %>%
    dplyr::collect()

  expect_true(nrow(res) > 0)
})

add_shared_countycd_columns <- function(con) {
  DBI::dbExecute(con, "ALTER TABLE COND ADD COLUMN COUNTYCD DOUBLE")
  DBI::dbExecute(con, "UPDATE COND SET COUNTYCD = CASE WHEN CONDID = 1 THEN 10 ELSE 20 END")

  DBI::dbExecute(con, "ALTER TABLE TREE ADD COLUMN COUNTYCD DOUBLE")
  DBI::dbExecute(con, "UPDATE TREE SET COUNTYCD = CASE WHEN SPCD = 1 THEN 100 ELSE 200 END")
}

# Tests for new scoped API
test_that("transform() with tree() helper adds mutations", {
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001)

  # New API: transform(tree(...))
  result <- handler %>%
    transform(tree(BA = 0.005454 * DIA^2)) %>%
    aggregate(tree(BA)) %>%
    dplyr::collect()

  # Old API (for comparison): mutate_tree(...)
  result_old <- eval_handler(con, evalid = 1001) %>%
    mutate_tree(BA = 0.005454 * DIA^2) %>%
    aggregate(tree(BA)) %>%
    dplyr::collect()

  # Should produce identical results
  expect_equal(nrow(result), nrow(result_old))
  expect_equal(sum(result$BA, na.rm = TRUE), sum(result_old$BA, na.rm = TRUE))
})

test_that("subset() with tree() helper adds filters", {
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001)

  # New API: subset(tree(...)) - use SPCD which exists in TREE table
  result_new <- handler %>%
    subset(tree(SPCD == 1)) %>%
    aggregate(tree(VOLCFGRS)) %>%
    dplyr::collect()

  # Old API (for comparison): filter_tree(...)
  result_old <- eval_handler(con, evalid = 1001) %>%
    filter_tree(SPCD == 1) %>%
    aggregate(tree(VOLCFGRS)) %>%
    dplyr::collect()

  # Should produce identical results
  expect_equal(nrow(result_new), nrow(result_old))
  expect_equal(sum(result_new$VOLCFGRS, na.rm = TRUE), sum(result_old$VOLCFGRS, na.rm = TRUE))
})

test_that("partition() with tree() helper sets domains", {
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001)

  # New API: partition(tree(...))
  result_new <- handler %>%
    partition(tree(SPCD)) %>%
    aggregate(tree(VOLCFGRS)) %>%
    dplyr::collect() %>%
    dplyr::distinct(SPCD) %>%
    nrow()

  # Old API (for comparison): set_tree_domains(...)
  result_old <- eval_handler(con, evalid = 1001) %>%
    set_tree_domains(SPCD) %>%
    aggregate(tree(VOLCFGRS)) %>%
    dplyr::collect() %>%
    dplyr::distinct(SPCD) %>%
    nrow()

  # Should produce same number of domain levels
  expect_equal(result_new, result_old)
})

test_that("aggregate() accepts cond() helper targets", {
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  result_new <- eval_handler(con, evalid = 1001) %>%
    partition(cond(COND_STATUS_CD)) %>%
    aggregate(cond()) %>%
    dplyr::collect() %>%
    dplyr::arrange(COND_STATUS_CD, PLT_CN, PLOT)

  result_with_placeholder <- eval_handler(con, evalid = 1001) %>%
    partition(cond(COND_STATUS_CD)) %>%
    aggregate(cond(1)) %>%
    dplyr::collect() %>%
    dplyr::arrange(COND_STATUS_CD, PLT_CN, PLOT)

  expect_equal(result_new, result_with_placeholder)
})

test_that("aggregate() preserves user-defined output names", {
  con <- setup_test_db()
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
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  result_new <- eval_handler(con, evalid = 1001) %>%
    partition(tree(SPCD), cond(COND_STATUS_CD)) %>%
    aggregate(tree(VOLCFGRS)) %>%
    dplyr::collect() %>%
    dplyr::arrange(COND_STATUS_CD, SPCD, PLT_CN, PLOT)

  result_old <- eval_handler(con, evalid = 1001) %>%
    set_tree_domains(SPCD) %>%
    set_cond_domains(COND_STATUS_CD) %>%
    aggregate(tree(VOLCFGRS)) %>%
    dplyr::collect() %>%
    dplyr::arrange(COND_STATUS_CD, SPCD, PLT_CN, PLOT)

  expect_equal(result_new, result_old)
})

test_that("partition(plot(COUNTYCD)) works for tree and cond aggregation", {
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  tree_result <- eval_handler(con, evalid = 1001) %>%
    partition(plot(COUNTYCD)) %>%
    aggregate(tree(VOLCFGRS)) %>%
    dplyr::collect()

  cond_result <- eval_handler(con, evalid = 1001) %>%
    partition(plot(COUNTYCD)) %>%
    aggregate(cond()) %>%
    dplyr::collect()

  expect_true("COUNTYCD" %in% colnames(tree_result))
  expect_true("COUNTYCD" %in% colnames(cond_result))
  expect_equal(sort(unique(tree_result$COUNTYCD)), 1)
  expect_equal(sort(unique(cond_result$COUNTYCD)), 1)
})

test_that("partition() respects helper scope for shared column names", {
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  add_shared_countycd_columns(con)

  cond_result <- eval_handler(con, evalid = 1001) %>%
    partition(cond(COUNTYCD)) %>%
    aggregate(cond()) %>%
    dplyr::collect()

  tree_result <- eval_handler(con, evalid = 1001) %>%
    partition(tree(COUNTYCD)) %>%
    aggregate(tree(VOLCFGRS)) %>%
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
  con <- setup_test_db()
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
