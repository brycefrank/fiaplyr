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
    estimate(cond())

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

test_that("margins=TRUE for cond adds grand total row and full rows", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001) |>
    partition(cond(COND_STATUS_CD))
  pe <- PostStratifiedEstimator(handler)

  res_margins <- estimate(pe, cond(), margins = TRUE)
  res_full    <- estimate(pe, cond(), margins = FALSE)

  # Grand total rows have NA for COND_STATUS_CD
  grand_total <- res_margins[is.na(res_margins$COND_STATUS_CD), ]
  full_rows   <- res_margins[!is.na(res_margins$COND_STATUS_CD), ]

  # Grand total should have exactly one row (one target variable "prop")
  expect_equal(nrow(grand_total), 1L)

  # Grand total estimate should equal 1 (sum of proportions = 1)
  expect_equal(grand_total$estimate, 1, tolerance = 1e-10)

  # Full rows should match the non-margins result
  full_rows_sorted   <- full_rows[order(full_rows$COND_STATUS_CD), ]
  res_full_sorted    <- res_full[order(res_full$COND_STATUS_CD), ]
  expect_equal(full_rows_sorted$estimate, res_full_sorted$estimate)
  expect_equal(full_rows_sorted$se,       res_full_sorted$se)
})

test_that("margins=TRUE for tree produces correct domain subsets", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001) |>
    partition(tree(SPCD), cond(COND_STATUS_CD))
  pe <- PostStratifiedEstimator(handler)

  res_margins <- estimate(pe, tree(VOLCFGRS), margins = TRUE)

  # With 1 tree domain and 1 cond domain there are 2^2 = 4 subsets.
  # Each produces one row per unique domain-value combo for that subset.
  # The grand total (both domains NA) should be exactly 1 row.
  grand_total <- res_margins[is.na(res_margins$SPCD) & is.na(res_margins$COND_STATUS_CD), ]
  expect_equal(nrow(grand_total), 1L)

  # SPCD-only marginal rows: SPCD is set, COND_STATUS_CD is NA
  spcd_marginal <- res_margins[!is.na(res_margins$SPCD) & is.na(res_margins$COND_STATUS_CD), ]
  expect_true(nrow(spcd_marginal) > 0)

  # COND_STATUS_CD-only marginal rows: COND_STATUS_CD is set, SPCD is NA
  cond_marginal <- res_margins[is.na(res_margins$SPCD) & !is.na(res_margins$COND_STATUS_CD), ]
  expect_true(nrow(cond_marginal) > 0)

  # Full cross rows: both domains set
  full_rows <- res_margins[!is.na(res_margins$SPCD) & !is.na(res_margins$COND_STATUS_CD), ]
  expect_true(nrow(full_rows) > 0)
})

test_that("marginal estimates match direct re-estimation with reduced domains", {
  con <- setup_status_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler_both <- eval_handler(con, evalid = 1001) |>
    partition(tree(SPCD), cond(COND_STATUS_CD))
  pe_both <- PostStratifiedEstimator(handler_both)

  res_margins <- estimate(pe_both, tree(VOLCFGRS), margins = TRUE)

  # SPCD-only marginal from margins=TRUE
  spcd_margin <- res_margins[!is.na(res_margins$SPCD) & is.na(res_margins$COND_STATUS_CD), ]
  spcd_margin <- spcd_margin[order(spcd_margin$SPCD), ]

  # Direct estimate with only SPCD domain
  handler_spcd <- eval_handler(con, evalid = 1001) |>
    partition(tree(SPCD))
  pe_spcd <- PostStratifiedEstimator(handler_spcd)
  res_spcd <- estimate(pe_spcd, tree(VOLCFGRS))
  res_spcd <- res_spcd[order(res_spcd$SPCD), ]

  expect_equal(spcd_margin$estimate, res_spcd$estimate, tolerance = 1e-10)
  expect_equal(spcd_margin$se,       res_spcd$se,       tolerance = 1e-10)

  # Grand total marginal should match estimate with no domains
  grand_total <- res_margins[is.na(res_margins$SPCD) & is.na(res_margins$COND_STATUS_CD), ]
  handler_none <- eval_handler(con, evalid = 1001)
  pe_none <- PostStratifiedEstimator(handler_none)
  res_none <- estimate(pe_none, tree(VOLCFGRS))

  expect_equal(grand_total$estimate, res_none$estimate, tolerance = 1e-10)
  expect_equal(grand_total$se,       res_none$se,       tolerance = 1e-10)
})


test_that("tree_history estimates apply SUBPTYP adjustment factors", {
  con <- setup_grm_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler_base <- eval_handler(con, evalid = 1003, spec = grm_analysis())
  pe_base <- PostStratifiedEstimator(handler_base)

  base_est <- estimate(pe_base, tree_history(1), output = "total") |>
    dplyr::pull(estimate)

  DBI::dbExecute(con, "UPDATE POP_STRATUM SET ADJ_FACTOR_SUBP = 2.0, ADJ_FACTOR_MICR = 0.5, ADJ_FACTOR_MACR = 1.5")

  handler_adj <- eval_handler(con, evalid = 1003, spec = grm_analysis())
  pe_adj <- PostStratifiedEstimator(handler_adj)

  adj_est <- estimate(pe_adj, tree_history(1), output = "total") |>
    dplyr::pull(estimate)

  expect_false(isTRUE(all.equal(base_est, adj_est)))
})

test_that("GRM basis selects matching TREE_GRM_COMPONENT subtype columns", {
  con <- setup_grm_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler_al <- eval_handler(con, evalid = 1003, spec = grm_analysis(tree_basis = "all_live", land_basis = "forest_land"))
  handler_gs <- eval_handler(con, evalid = 1003, spec = grm_analysis(tree_basis = "growing_stock", land_basis = "timberland"))

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
