test_that("PostStratifiedEstimator estimates correct forested area", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # Create Handlers
  # For cond: Group by COND_STATUS_CD
  handler <- eval_handler(con, evalid = 1001) |>
    partition(cond(COND_STATUS_CD))

  # Create Estimator
  pe <- PostStratifiedEstimator(handler)

  # Estimate area for COND_STATUS_CD
  res <- estimate(pe, cond()) |>
    dplyr::collect()

  # Verify structure
  expected_cols <- c("COND_STATUS_CD", "var", "estimate", "se")
  expect_true(all(expected_cols %in% colnames(res)))

  # Expect forested (COND_STATUS_CD = 1) estimate
  # Verify that COND_STATUS_CD 1 and 2 are present
  expect_true(1 %in% res$COND_STATUS_CD)
  expect_true(2 %in% res$COND_STATUS_CD)

  # Ensure props sum to 1
  expect_equal(sum(res$estimate), 1)

  # Ensure variance is valid
  expect_true(is.numeric(res$estimate))
  expect_true(is.numeric(res$se))
})

test_that("condition estimates support MACRO condition proportion basis", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  DBI::dbExecute(con, "UPDATE COND SET PROP_BASIS = 'MACRO'")

  handler <- eval_handler(con, evalid = 1001) |>
    partition(cond(COND_STATUS_CD))

  res <- handler |>
    PostStratifiedEstimator() |>
    estimate(cond()) |>
    dplyr::collect()

  expect_false(any(is.na(res$estimate)))
  expect_false(any(is.na(res$se)))
  expect_equal(sum(res$estimate), 1)
})

test_that("PostStratifiedEstimator supports mean and total outputs", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001) |>
    partition(cond(COND_STATUS_CD))

  pe <- PostStratifiedEstimator(handler)

  res_mean <- estimate(pe, cond(), output = "mean") |>
    dplyr::collect()

  res_total <- estimate(pe, cond(), output = "total") |>
    dplyr::collect()

  total_area <- handler@tables$pop_estn_unit %>%
    dplyr::summarise(area = sum(AREA_USED, na.rm = TRUE)) %>%
    dplyr::collect() %>%
    dplyr::pull(area)

  joined <- dplyr::inner_join(
    res_mean,
    res_total,
    by = c("COND_STATUS_CD", "var"),
    suffix = c("_mean", "_total")
  )

  expect_equal(joined$estimate_total, joined$estimate_mean * total_area)
  expect_equal(joined$se_total, joined$se_mean * total_area)
})

test_that("estimate() preserves user-defined target names", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  tree_handler <- eval_handler(con, evalid = 1001)
  tree_res <- tree_handler |>
    PostStratifiedEstimator() |>
    estimate(tree(my_vol = VOLCFGRS)) |>
    dplyr::collect()

  expect_equal(unique(tree_res$var), "my_vol")

  cond_handler <- eval_handler(con, evalid = 1001) |>
    partition(cond(COND_STATUS_CD))
  cond_res <- cond_handler |>
    PostStratifiedEstimator() |>
    estimate(cond(my_prop = 1)) |>
    dplyr::collect()

  expect_equal(unique(cond_res$var), "my_prop")
})

test_that("estimate(tree()) estimates trees per acre by default", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001)
  pe <- PostStratifiedEstimator(handler)

  res <- estimate(pe, tree()) |>
    dplyr::collect()
  explicit_res <- estimate(pe, tree(1)) |>
    dplyr::collect()

  expect_equal(unique(res$var), "tree_count")
  expect_equal(res$estimate, explicit_res$estimate)
  expect_equal(res$se, explicit_res$se)
})

test_that("status tree estimates apply DIA-based adjustment factors for bare targets", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  DBI::dbExecute(con, "ALTER TABLE PLOT ADD COLUMN MACRO_BREAKPOINT_DIA DOUBLE")
  DBI::dbExecute(con, "UPDATE PLOT SET MACRO_BREAKPOINT_DIA = 11.0")

  DBI::dbExecute(con, "UPDATE TREE SET TPA_UNADJ = 1.0, VOLCFNET = 1.0")
  DBI::dbExecute(
    con,
    "UPDATE TREE SET DIA = CASE CN
      WHEN 1 THEN 3.0
      WHEN 2 THEN 10.0
      WHEN 3 THEN 12.0
      WHEN 4 THEN NULL
      WHEN 5 THEN 3.0
      WHEN 6 THEN 10.0
      WHEN 7 THEN 12.0
      WHEN 8 THEN NULL
      ELSE DIA END"
  )

  handler_base <- eval_handler(con, evalid = 1001)
  pe_base <- PostStratifiedEstimator(handler_base)
  base_est <- estimate(pe_base, tree(VOLCFNET), output = "total") |>
    dplyr::pull(estimate)

  DBI::dbExecute(
    con,
    "UPDATE POP_STRATUM SET ADJ_FACTOR_SUBP = 2.0, ADJ_FACTOR_MICR = 3.0, ADJ_FACTOR_MACR = 5.0"
  )

  handler_adj <- eval_handler(con, evalid = 1001)
  pe_adj <- PostStratifiedEstimator(handler_adj)
  adj_est <- estimate(pe_adj, tree(VOLCFNET), output = "total") |>
    dplyr::pull(estimate)

  expect_true(adj_est > base_est)
})

test_that("margins=TRUE for cond adds grand total row and full rows", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001) |>
    partition(cond(COND_STATUS_CD))
  pe <- PostStratifiedEstimator(handler)

  res_margins <- estimate(pe, cond(), margins = TRUE) |>
    dplyr::collect()
  res_full <- estimate(pe, cond(), margins = FALSE) |>
    dplyr::collect()

  # Grand total rows have NA for COND_STATUS_CD
  grand_total <- res_margins[is.na(res_margins$COND_STATUS_CD), ]
  full_rows <- res_margins[!is.na(res_margins$COND_STATUS_CD), ]

  # Grand total should have exactly one row (one target variable "prop")
  expect_equal(nrow(grand_total), 1L)

  # Grand total estimate should equal 1 (sum of proportions = 1)
  expect_equal(grand_total$estimate, 1, tolerance = 1e-10)

  # Full rows should match the non-margins result
  full_rows_sorted <- full_rows[order(full_rows$COND_STATUS_CD), ]
  res_full_sorted <- res_full[order(res_full$COND_STATUS_CD), ]
  expect_equal(full_rows_sorted$estimate, res_full_sorted$estimate)
  expect_equal(full_rows_sorted$se, res_full_sorted$se)
})

test_that("margins=TRUE for tree produces correct domain subsets", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001) |>
    partition(tree(SPCD), cond(COND_STATUS_CD))
  pe <- PostStratifiedEstimator(handler)

  res_margins <- estimate(pe, tree(VOLCFGRS), margins = TRUE) |>
    dplyr::collect()

  # With 1 tree domain and 1 cond domain there are 2^2 = 4 subsets.
  # Each produces one row per unique domain-value combo for that subset.
  # The grand total (both domains NA) should be exactly 1 row.
  grand_total <- res_margins[
    is.na(res_margins$SPCD) & is.na(res_margins$COND_STATUS_CD),
  ]
  expect_equal(nrow(grand_total), 1L)

  # SPCD-only marginal rows: SPCD is set, COND_STATUS_CD is NA
  spcd_marginal <- res_margins[
    !is.na(res_margins$SPCD) & is.na(res_margins$COND_STATUS_CD),
  ]
  expect_true(nrow(spcd_marginal) > 0)

  # COND_STATUS_CD-only marginal rows: COND_STATUS_CD is set, SPCD is NA
  cond_marginal <- res_margins[
    is.na(res_margins$SPCD) & !is.na(res_margins$COND_STATUS_CD),
  ]
  expect_true(nrow(cond_marginal) > 0)

  # Full cross rows: both domains set
  full_rows <- res_margins[
    !is.na(res_margins$SPCD) & !is.na(res_margins$COND_STATUS_CD),
  ]
  expect_true(nrow(full_rows) > 0)
})

test_that("marginal estimates match direct re-estimation with reduced domains", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler_both <- eval_handler(con, evalid = 1001) |>
    partition(tree(SPCD), cond(COND_STATUS_CD))
  pe_both <- PostStratifiedEstimator(handler_both)

  res_margins <- estimate(pe_both, tree(VOLCFGRS), margins = TRUE) |>
    dplyr::collect()

  # SPCD-only marginal from margins=TRUE
  spcd_margin <- res_margins[
    !is.na(res_margins$SPCD) & is.na(res_margins$COND_STATUS_CD),
  ]
  spcd_margin <- spcd_margin[order(spcd_margin$SPCD), ]

  # Direct estimate with only SPCD domain
  handler_spcd <- eval_handler(con, evalid = 1001) |>
    partition(tree(SPCD))
  pe_spcd <- PostStratifiedEstimator(handler_spcd)
  res_spcd <- estimate(pe_spcd, tree(VOLCFGRS)) |>
    dplyr::collect()
  res_spcd <- res_spcd[order(res_spcd$SPCD), ]

  expect_equal(spcd_margin$estimate, res_spcd$estimate, tolerance = 1e-10)
  expect_equal(spcd_margin$se, res_spcd$se, tolerance = 1e-10)

  # Grand total marginal should match estimate with no domains
  grand_total <- res_margins[
    is.na(res_margins$SPCD) & is.na(res_margins$COND_STATUS_CD),
  ]
  handler_none <- eval_handler(con, evalid = 1001)
  pe_none <- PostStratifiedEstimator(handler_none)
  res_none <- estimate(pe_none, tree(VOLCFGRS)) |>
    dplyr::collect()

  expect_equal(grand_total$estimate, res_none$estimate, tolerance = 1e-10)
  expect_equal(grand_total$se, res_none$se, tolerance = 1e-10)
})

test_that("margins=TRUE with no domains returns a single grand total row", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001)
  pe <- PostStratifiedEstimator(handler)

  res_margins <- estimate(pe, tree(VOLCFGRS), margins = TRUE) |>
    dplyr::collect()
  res_plain <- estimate(pe, tree(VOLCFGRS), margins = FALSE) |>
    dplyr::collect()

  expect_equal(nrow(res_margins), nrow(res_plain))
  expect_equal(res_margins$estimate, res_plain$estimate, tolerance = 1e-10)
  expect_equal(res_margins$se, res_plain$se, tolerance = 1e-10)
})

test_that("is_marginal column is absent when margins=FALSE", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001) |>
    partition(tree(SPCD), cond(COND_STATUS_CD))
  pe <- PostStratifiedEstimator(handler)

  res_tree <- estimate(pe, tree(VOLCFGRS), margins = FALSE)
  expect_false("is_marginal" %in% colnames(res_tree))

  res_cond <- estimate(pe, cond(), margins = FALSE)
  expect_false("is_marginal" %in% colnames(res_cond))
})

test_that("is_marginal correctly flags marginal vs full-domain rows", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001) |>
    partition(tree(SPCD), cond(COND_STATUS_CD))
  pe <- PostStratifiedEstimator(handler)

  res <- estimate(pe, tree(VOLCFGRS), margins = TRUE) |>
    dplyr::collect()

  expect_true("is_marginal" %in% colnames(res))

  # Only rows with both domains set are non-marginal
  full_rows <- res[!is.na(res$SPCD) & !is.na(res$COND_STATUS_CD), ]
  expect_true(all(!full_rows$is_marginal))

  # Rows with any domain collapsed (NA) are marginal
  marginal_rows <- res[is.na(res$SPCD) | is.na(res$COND_STATUS_CD), ]
  expect_true(all(marginal_rows$is_marginal))
})

test_that("is_marginal correctly flags cond marginal rows", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001) |>
    partition(cond(COND_STATUS_CD))
  pe <- PostStratifiedEstimator(handler)

  res <- estimate(pe, cond(), margins = TRUE) |>
    dplyr::collect()

  expect_true("is_marginal" %in% colnames(res))

  full_rows <- res[!is.na(res$COND_STATUS_CD), ]
  marginal_rows <- res[is.na(res$COND_STATUS_CD), ]

  expect_true(all(!full_rows$is_marginal))
  expect_true(all(marginal_rows$is_marginal))
})

test_that("PostStratifiedEstimator supports tree_history estimates for GRM handlers", {
  con <- setup_grm_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1003, spec = new("GRMAnalysis"))
  pe <- PostStratifiedEstimator(handler)

  res <- estimate(pe, tree_history(VOLCFNET)) |>
    dplyr::collect()

  expect_true(nrow(res) > 0)
  expect_true(all(c("estimate", "se", "var") %in% colnames(res)))
  expect_false("is_marginal" %in% colnames(res))
})

test_that("tree_history estimates support call expressions as targets", {
  con <- setup_grm_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1003, spec = new("GRMAnalysis"))
  pe <- PostStratifiedEstimator(handler)

  res <- estimate(
    pe,
    tree_history(log_vol = sum(log(VOLCFNET), na.rm = TRUE))
  ) |>
    dplyr::collect()

  expect_true(nrow(res) > 0)
  expect_true("log_vol" %in% unique(res$var))
})

test_that("tree_history margins use cond and tree_history domains", {
  con <- setup_grm_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1003, spec = new("GRMAnalysis")) |>
    partition(cond(COND_STATUS_CD), tree_history(SPCD))
  pe <- PostStratifiedEstimator(handler)

  res <- estimate(pe, tree_history(VOLCFNET), margins = TRUE) |>
    dplyr::collect()

  expect_true("is_marginal" %in% colnames(res))
  expect_true(any(is.na(res$COND_STATUS_CD)))
  expect_true(any(is.na(res$SPCD)))
  expect_equal(sum(is.na(res$COND_STATUS_CD) & is.na(res$SPCD)), 1L)
})

test_that("tree_history estimates apply SUBPTYP adjustment factors", {
  con <- setup_grm_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler_base <- eval_handler(con, evalid = 1003, spec = grm_analysis())
  pe_base <- PostStratifiedEstimator(handler_base)

  base_est <- estimate(pe_base, tree_history(1), output = "total") |>
    dplyr::pull(estimate)

  DBI::dbExecute(
    con,
    "UPDATE POP_STRATUM SET ADJ_FACTOR_SUBP = 2.0, ADJ_FACTOR_MICR = 0.5, ADJ_FACTOR_MACR = 1.5"
  )

  handler_adj <- eval_handler(con, evalid = 1003, spec = grm_analysis())
  pe_adj <- PostStratifiedEstimator(handler_adj)

  adj_est <- estimate(pe_adj, tree_history(1), output = "total") |>
    dplyr::pull(estimate)

  expect_false(isTRUE(all.equal(base_est, adj_est)))
})

test_that("tree_history GRM macros apply SUBPTYP adjustment factors", {
  con <- setup_grm_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler_base <- eval_handler(con, evalid = 1003, spec = grm_analysis())
  pe_base <- PostStratifiedEstimator(handler_base)

  base_est <- estimate(
    pe_base,
    tree_history(grm_mortality(1)),
    output = "total"
  ) |>
    dplyr::pull(estimate)

  DBI::dbExecute(
    con,
    "UPDATE POP_STRATUM SET ADJ_FACTOR_SUBP = 2.0, ADJ_FACTOR_MICR = 2.0, ADJ_FACTOR_MACR = 2.0"
  )

  handler_adj <- eval_handler(con, evalid = 1003, spec = grm_analysis())
  pe_adj <- PostStratifiedEstimator(handler_adj)

  adj_est <- estimate(
    pe_adj,
    tree_history(grm_mortality(1)),
    output = "total"
  ) |>
    dplyr::pull(estimate)

  expect_true(adj_est > base_est)
})

test_that("GRM basis selects matching TREE_GRM_COMPONENT subtype columns", {
  con <- setup_grm_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler_al <- eval_handler(
    con,
    evalid = 1003,
    spec = grm_analysis(tree_basis = "all_live", land_basis = "forest_land")
  )
  handler_gs <- eval_handler(
    con,
    evalid = 1003,
    spec = grm_analysis(tree_basis = "growing_stock", land_basis = "timberland")
  )

  al_subtypes <- materialize(handler_al, "tree_history") |>
    dplyr::select(SUBPTYP_GRM) |>
    dplyr::distinct() |>
    dplyr::collect() |>
    dplyr::pull(SUBPTYP_GRM)

  gs_subtypes <- materialize(handler_gs, "tree_history") |>
    dplyr::select(SUBPTYP_GRM) |>
    dplyr::distinct() |>
    dplyr::collect() |>
    dplyr::pull(SUBPTYP_GRM)

  expect_true(any(al_subtypes %in% c(1L, 2L, 3L, 9L)))
  expect_true(any(gs_subtypes %in% c(2L, 3L)))
  expect_false(setequal(sort(unique(al_subtypes)), sort(unique(gs_subtypes))))
})

test_that("unknown SUBPTYP_GRM codes do not create NA estimates", {
  con <- setup_grm_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler_unknown <- eval_handler(con, evalid = 1003, spec = grm_analysis())
  pe_unknown <- PostStratifiedEstimator(handler_unknown)

  est_with_unknown <- estimate(pe_unknown, tree_history(1), output = "total") |>
    dplyr::pull(estimate)

  DBI::dbExecute(
    con,
    "UPDATE TREE_GRM_COMPONENT SET SUBP_SUBPTYP_GRM_AL_FOREST = 1 WHERE TRE_CN = 8"
  )

  handler_known <- eval_handler(con, evalid = 1003, spec = grm_analysis())
  pe_known <- PostStratifiedEstimator(handler_known)

  est_without_unknown <- estimate(
    pe_known,
    tree_history(1),
    output = "total"
  ) |>
    dplyr::pull(estimate)

  expect_false(any(is.na(est_with_unknown)))
  expect_false(any(is.na(est_without_unknown)))
  expect_true(est_without_unknown > est_with_unknown)
})

test_that("tree_history estimates require GRMAnalysis handlers", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001)
  pe <- PostStratifiedEstimator(handler)

  expect_error(
    estimate(pe, tree_history(VOLCFNET)),
    "requires a GRMAnalysis handler"
  )
})
