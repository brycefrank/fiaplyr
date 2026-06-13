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
    aggregate(tree ~ VOLCFGRS) %>%
    dplyr::collect()

  woodland_filtered <- handler %>%
    filter_tree(WOODLAND != "N") %>%
    aggregate(tree ~ VOLCFGRS) %>%
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
        aggregate(tree ~ VOLCFGRS) %>%
        dplyr::collect()
      expect_true(nrow(res) > 0)
    },
    "REF_SPECIES table not available"
  )
})

# Tests for new scoped API
test_that("transform() with tree() helper adds mutations", {
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001)

  # New API: transform(tree(...))
  result <- handler %>%
    transform(tree(BA = 0.005454 * DIA^2)) %>%
    aggregate(tree ~ BA) %>%
    dplyr::collect()

  # Old API (for comparison): mutate_tree(...)
  result_old <- eval_handler(con, evalid = 1001) %>%
    mutate_tree(BA = 0.005454 * DIA^2) %>%
    aggregate(tree ~ BA) %>%
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
    aggregate(tree ~ VOLCFGRS) %>%
    dplyr::collect()

  # Old API (for comparison): filter_tree(...)
  result_old <- eval_handler(con, evalid = 1001) %>%
    filter_tree(SPCD == 1) %>%
    aggregate(tree ~ VOLCFGRS) %>%
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
    aggregate(tree ~ VOLCFGRS) %>%
    dplyr::collect() %>%
    dplyr::distinct(SPCD) %>%
    nrow()

  # Old API (for comparison): set_tree_domains(...)
  result_old <- eval_handler(con, evalid = 1001) %>%
    set_tree_domains(SPCD) %>%
    aggregate(tree ~ VOLCFGRS) %>%
    dplyr::collect() %>%
    dplyr::distinct(SPCD) %>%
    nrow()

  # Should produce same number of domain levels
  expect_equal(result_new, result_old)
})

test_that("partition() accepts multiple scoped helpers in one call", {
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  result_new <- eval_handler(con, evalid = 1001) %>%
    partition(tree(SPCD), cond(COND_STATUS_CD)) %>%
    aggregate(tree ~ VOLCFGRS) %>%
    dplyr::collect() %>%
    dplyr::arrange(COND_STATUS_CD, SPCD)

  result_old <- eval_handler(con, evalid = 1001) %>%
    set_tree_domains(SPCD) %>%
    set_cond_domains(COND_STATUS_CD) %>%
    aggregate(tree ~ VOLCFGRS) %>%
    dplyr::collect() %>%
    dplyr::arrange(COND_STATUS_CD, SPCD)

  expect_equal(result_new, result_old)
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
