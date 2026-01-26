test_that("EvalHandler initializes correctly", {
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001)

  expect_s4_class(handler, "EvalHandler")
  expect_equal(evalid(handler), 1001)

  # Check if tables are lazily loaded
  expect_true(dplyr::is.tbl(handler@plot))
  expect_true(dplyr::is.tbl(handler@tree))

  # Check content helper (simple check to see if join worked and valid data exists)
  plots <- handler@plot %>% dplyr::collect()
  expect_equal(nrow(plots), 4)

  trees <- handler@tree %>% dplyr::collect()
  expect_equal(nrow(trees), 4) # 4 trees in setup
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
