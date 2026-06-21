test_that("explore_evals lists evaluations", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # Add another evaluation
  DBI::dbWriteTable(con, "POP_EVAL", data.frame(
    CN = 2,
    EVALID = 2002,
    EVAL_DESCR = "Second Eval",
    stringsAsFactors = FALSE
  ), append = TRUE)

  res <- explore_evals(con)

  expect_true(dplyr::is.tbl(res) || is.data.frame(res))
  expect_equal(nrow(res), 2)
  expect_equal(colnames(res), c("EVALID", "EVAL_DESCR"))

  # Check content
  res_df <- as.data.frame(res)
  expect_true(1001 %in% res_df$EVALID)
  expect_true(2002 %in% res_df$EVALID)
  expect_true("Test Evaluation" %in% res_df$EVAL_DESCR)
  expect_true("Second Eval" %in% res_df$EVAL_DESCR)
})
