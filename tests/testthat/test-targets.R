test_that("GRM macros are fiaplyr_target objects in the tree_history scope", {
  m <- grm_mortality()
  expect_s3_class(m, "fiaplyr_target")
  expect_s3_class(m, "grm_target")
  expect_identical(m$scope, "tree_history")

  combined <- grm_net_change()
  expect_s3_class(combined, "fiaplyr_target")
  expect_identical(combined$scope, "tree_history")
})

test_that("DWM helpers are fiaplyr_target objects in the dwm scope", {
  d <- dwm_cwd(VOLCF)
  expect_s3_class(d, "fiaplyr_target")
  expect_s3_class(d, "dwm_target")
  expect_identical(d$scope, "dwm")

  f <- dwm_fwd(carbon = CARBON, size = "ALL")
  expect_s3_class(f, "fiaplyr_target")
  expect_identical(f$name, "carbon")
})

test_that("agg_expr() lowers both target kinds to summarise expressions", {
  m_deparse <- paste(rlang::expr_deparse(
    agg_expr(grm_mortality(1, adjust = "none"), adjusted = FALSE)
  ), collapse = " ")
  expect_true(rlang::is_call(
    agg_expr(grm_mortality(1, adjust = "none"), adjusted = FALSE),
    "sum"
  ))
  expect_match(m_deparse, "TPA_UNADJ_begin", fixed = TRUE)
  expect_match(m_deparse, "na.rm = TRUE", fixed = TRUE)

  d_expr <- agg_expr(dwm_cwd(DRYBIO), adjusted = FALSE)
  d_deparse <- paste(rlang::expr_deparse(d_expr), collapse = " ")
  expect_true(rlang::is_call(d_expr, "sum"))
  expect_match(d_deparse, "CWD_DRYBIO_UNADJ", fixed = TRUE)
  expect_match(d_deparse, "coalesce", fixed = TRUE)

  d_adj_deparse <- paste(rlang::expr_deparse(
    agg_expr(dwm_cwd(DRYBIO), adjusted = TRUE)
  ), collapse = " ")
  expect_match(d_adj_deparse, "CWD_DRYBIO_ADJ", fixed = TRUE)
})

test_that("targets understand the scoped helper within which they are called", {
  expect_error(
    .parse_target_spec(grm_mortality(), "test"),
    "tree_history"
  )
  expect_error(
    .parse_target_spec(dwm_cwd(VOLCF), "test"),
    "wrapped in `dwm"
  )
})
