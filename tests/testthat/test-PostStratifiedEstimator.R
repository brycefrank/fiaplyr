test_that("PostStratifiedEstimator estimates correct forested area", {
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  # Create Handlers
  # For cond: Group by COND_STATUS_CD
  handler <- eval_handler(con, evalid = 1001) |>
    set_cond_domains(COND_STATUS_CD)

  # Create Estimator
  pe <- PostStratifiedEstimator(handler)

  # Estimate area for COND_STATUS_CD
  res <- estimate(pe, cond ~ 1) |>
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

test_that("PostStratifiedEstimator supports mean and total outputs", {
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001) |>
    set_cond_domains(COND_STATUS_CD)

  pe <- PostStratifiedEstimator(handler)

  res_mean <- estimate(pe, cond ~ 1, output = "mean") |>
    dplyr::collect()

  res_total <- estimate(pe, cond ~ 1, output = "total") |>
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
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001) |>
    set_cond_domains(COND_STATUS_CD)
  pe <- PostStratifiedEstimator(handler)

  res_margins <- estimate(pe, cond ~ 1, margins = TRUE)
  res_full    <- estimate(pe, cond ~ 1, margins = FALSE)

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
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001) |>
    set_tree_domains(SPCD) |>
    set_cond_domains(COND_STATUS_CD)
  pe <- PostStratifiedEstimator(handler)

  res_margins <- estimate(pe, tree ~ VOLCFGRS, margins = TRUE)

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
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler_both <- eval_handler(con, evalid = 1001) |>
    set_tree_domains(SPCD) |>
    set_cond_domains(COND_STATUS_CD)
  pe_both <- PostStratifiedEstimator(handler_both)

  res_margins <- estimate(pe_both, tree ~ VOLCFGRS, margins = TRUE)

  # SPCD-only marginal from margins=TRUE
  spcd_margin <- res_margins[!is.na(res_margins$SPCD) & is.na(res_margins$COND_STATUS_CD), ]
  spcd_margin <- spcd_margin[order(spcd_margin$SPCD), ]

  # Direct estimate with only SPCD domain
  handler_spcd <- eval_handler(con, evalid = 1001) |>
    set_tree_domains(SPCD)
  pe_spcd <- PostStratifiedEstimator(handler_spcd)
  res_spcd <- estimate(pe_spcd, tree ~ VOLCFGRS)
  res_spcd <- res_spcd[order(res_spcd$SPCD), ]

  expect_equal(spcd_margin$estimate, res_spcd$estimate, tolerance = 1e-10)
  expect_equal(spcd_margin$se,       res_spcd$se,       tolerance = 1e-10)

  # Grand total marginal should match estimate with no domains
  grand_total <- res_margins[is.na(res_margins$SPCD) & is.na(res_margins$COND_STATUS_CD), ]
  handler_none <- eval_handler(con, evalid = 1001)
  pe_none <- PostStratifiedEstimator(handler_none)
  res_none <- estimate(pe_none, tree ~ VOLCFGRS)

  expect_equal(grand_total$estimate, res_none$estimate, tolerance = 1e-10)
  expect_equal(grand_total$se,       res_none$se,       tolerance = 1e-10)
})

test_that("margins=TRUE with no domains returns a single grand total row", {
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001)
  pe <- PostStratifiedEstimator(handler)

  res_margins <- estimate(pe, tree ~ VOLCFGRS, margins = TRUE)
  res_plain   <- estimate(pe, tree ~ VOLCFGRS, margins = FALSE)

  expect_equal(nrow(res_margins), nrow(res_plain))
  expect_equal(res_margins$estimate, res_plain$estimate, tolerance = 1e-10)
  expect_equal(res_margins$se,       res_plain$se,       tolerance = 1e-10)
})

test_that("is_marginal column is absent when margins=FALSE", {
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001) |>
    set_tree_domains(SPCD) |>
    set_cond_domains(COND_STATUS_CD)
  pe <- PostStratifiedEstimator(handler)

  res_tree <- estimate(pe, tree ~ VOLCFGRS, margins = FALSE)
  expect_false("is_marginal" %in% colnames(res_tree))

  res_cond <- estimate(pe, cond ~ 1, margins = FALSE)
  expect_false("is_marginal" %in% colnames(res_cond))
})

test_that("is_marginal correctly flags marginal vs full-domain rows", {
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001) |>
    set_tree_domains(SPCD) |>
    set_cond_domains(COND_STATUS_CD)
  pe <- PostStratifiedEstimator(handler)

  res <- estimate(pe, tree ~ VOLCFGRS, margins = TRUE)

  expect_true("is_marginal" %in% colnames(res))

  # Only rows with both domains set are non-marginal
  full_rows <- res[!is.na(res$SPCD) & !is.na(res$COND_STATUS_CD), ]
  expect_true(all(!full_rows$is_marginal))

  # Rows with any domain collapsed (NA) are marginal
  marginal_rows <- res[is.na(res$SPCD) | is.na(res$COND_STATUS_CD), ]
  expect_true(all(marginal_rows$is_marginal))
})

test_that("is_marginal correctly flags cond marginal rows", {
  con <- setup_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, evalid = 1001) |>
    set_cond_domains(COND_STATUS_CD)
  pe <- PostStratifiedEstimator(handler)

  res <- estimate(pe, cond ~ 1, margins = TRUE)

  expect_true("is_marginal" %in% colnames(res))

  full_rows     <- res[!is.na(res$COND_STATUS_CD), ]
  marginal_rows <- res[is.na(res$COND_STATUS_CD), ]

  expect_true(all(!full_rows$is_marginal))
  expect_true(all(marginal_rows$is_marginal))
})
