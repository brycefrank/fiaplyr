test_that("EvalHandler throws error for non-existent evalid", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # Test with an EVALID that definitely does not exist in the setup_status_test_db
  # setup_status_test_db creates EVALID 1001
  expect_error(eval_handler(con, evalid = 999999), "does not exist")
})
