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
