test_that("dwm_analysis initializes filtered lazy tables", {
  con <- setup_dwm_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, 1001, spec = dwm_analysis())

  expect_s4_class(handler@spec, "DWMAnalysis")
  expect_s4_class(handler@spec, "AnalysisSpec")
  expect_true(inherits(handler@tables$cond_dwm_calc, "tbl_lazy"))
  expect_equal(
    handler@tables$cond_dwm_calc %>% dplyr::collect() %>% nrow(),
    8
  )
})

test_that("DWM initialization supports mappings and reports schema errors", {
  con <- setup_dwm_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  DBI::dbExecute(con, "ALTER TABLE COND_DWM_CALC RENAME TO CUSTOM_DWM")
  mapping <- database_mapping(table_map = list(COND_DWM_CALC = "CUSTOM_DWM"))
  expect_s4_class(
    eval_handler(con, 1001, spec = dwm_analysis(), backend = mapping),
    "EvalHandler"
  )

  DBI::dbExecute(con, "ALTER TABLE CUSTOM_DWM RENAME TO BROKEN_DWM")
  expect_error(
    eval_handler(con, 1001, spec = dwm_analysis(), backend = mapping),
    "required.*not available"
  )

  DBI::dbExecute(con, "ALTER TABLE BROKEN_DWM RENAME TO CUSTOM_DWM")
  DBI::dbExecute(con, "ALTER TABLE CUSTOM_DWM DROP COLUMN EVALID")
  expect_error(
    eval_handler(con, 1001, spec = dwm_analysis(), backend = mapping),
    "missing required column.*EVALID"
  )
})

test_that("DWM plot aggregation uses unadjusted values and fills missing plots", {
  con <- setup_dwm_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  result <- eval_handler(con, 1001, spec = dwm_analysis()) %>%
    aggregate(dwm(dwm_cwd(volume = VOLCF))) %>%
    dplyr::collect() %>%
    dplyr::arrange(PLT_CN)

  expect_equal(nrow(result), 8)
  expect_equal(result$volume[result$PLT_CN == 104], 45)
  expect_equal(result$volume[result$PLT_CN == 102], 0)
  expect_equal(result$volume[result$PLT_CN == 202], 0)
})

test_that("DWM plot filters are preserved by the complete scaffold", {
  con <- setup_dwm_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  result <- eval_handler(con, 1001, spec = dwm_analysis()) %>%
    subset(plot(PLOT == 1)) %>%
    aggregate(dwm(dwm_cwd(VOLCF))) %>%
    dplyr::collect()

  expect_equal(nrow(result), 1)
  expect_identical(result$PLOT, 1)
})

test_that("DWM aggregation scales biomass and combines FWD sizes", {
  con <- setup_dwm_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, 1001, spec = dwm_analysis())
  biomass <- handler %>%
    aggregate(dwm(dwm_cwd(DRYBIO))) %>%
    dplyr::collect()
  fwd <- handler %>%
    aggregate(dwm(dwm_fwd(total_carbon = CARBON, size = "ALL"))) %>%
    dplyr::collect()

  expect_equal(biomass$dwm_cwd_DRYBIO[biomass$PLT_CN == 101], 10 / 2000)
  expect_equal(fwd$total_carbon[fwd$PLT_CN == 101], 60 / 2000)
  expect_equal(fwd$total_carbon[fwd$PLT_CN == 104], 260 / 2000)
})

test_that("DWM scoped pipeline operations compose lazily", {
  con <- setup_dwm_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  phase_names <- data.frame(PHASE = c("P2", "P3"), phase_name = c("two", "three"))
  handler <- eval_handler(con, 1001, spec = dwm_analysis()) %>%
    augment(dwm(phase_names, by = "PHASE")) %>%
    transform(dwm(double_volume = CWD_VOLCF_UNADJ * 2)) %>%
    subset(dwm(double_volume >= 20)) %>%
    partition(cond(FORTYPCD), dwm(phase_name))

  materialized <- suppressWarnings(materialize(handler, "dwm"))
  result <- suppressWarnings(
    handler %>%
      aggregate(dwm(dwm_cwd(VOLCF)), sparse = TRUE) %>%
      dplyr::collect()
  )

  expect_true(inherits(materialized, "tbl_lazy"))
  expect_true(all(c("double_volume", "phase_name") %in% colnames(materialized)))
  expect_true(all(c("FORTYPCD", "phase_name") %in% colnames(result)))
})

test_that("DWM point estimates use adjusted values", {
  con <- setup_dwm_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  result <- eval_handler(con, 1001, spec = dwm_analysis()) %>%
    estimate(dwm(dwm_cwd(volume = VOLCF))) %>%
    dplyr::collect()

  expect_identical(result$var, "volume")
  expect_equal(result$estimate, 70.8333333, tolerance = 1e-6)
  expect_true(is.finite(result$se))
})

test_that("DWM estimates support domains, totals, margins, and ratios", {
  con <- setup_dwm_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, 1001, spec = dwm_analysis()) %>%
    partition(cond(FORTYPCD), dwm(PHASE))

  total <- handler %>%
    estimate(dwm(dwm_cwd(CARBON)), output = "total", margins = TRUE) %>%
    dplyr::collect()
  ratio_result <- handler %>%
    estimate(ratio(dwm(dwm_cwd(CARBON)), cond())) %>%
    dplyr::collect()

  expect_true(all(c("FORTYPCD", "PHASE", "is_marginal") %in% colnames(total)))
  expect_true(any(total$is_marginal))
  expect_true(any(!total$is_marginal))
  expect_true(all(c("var_n", "var_d", "estimate", "se") %in% colnames(ratio_result)))
  expect_true(all(is.finite(ratio_result$estimate)))
  expect_true(all(is.finite(ratio_result$se)))
})

test_that("DWM targets work on either side of ratio estimation", {
  con <- setup_dwm_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, 1001, spec = dwm_analysis())
  dwm_denominator <- handler %>%
    estimate(ratio(cond(), dwm(dwm_cwd(VOLCF)))) %>%
    dplyr::collect()
  same_scope <- handler %>%
    estimate(ratio(
      dwm(dwm_cwd(carbon = CARBON)),
      dwm(dwm_cwd(volume = VOLCF))
    )) %>%
    dplyr::collect()

  expect_identical(dwm_denominator$var_n, "prop")
  expect_identical(dwm_denominator$var_d, "dwm_cwd_VOLCF")
  expect_identical(same_scope$var_n, "carbon")
  expect_identical(same_scope$var_d, "volume")
  expect_true(all(is.finite(dwm_denominator$se)))
  expect_true(all(is.finite(same_scope$se)))
})

test_that("DWM analysis rejects unsupported targets and missing source fields", {
  con <- setup_dwm_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, 1001, spec = dwm_analysis())
  expect_error(handler %>% aggregate(tree(VOLCFNET)), "supports DWM")
  expect_error(handler %>% aggregate(dwm(CWD_VOLCF_UNADJ)), "component helper")
  expect_error(handler %>% aggregate(dwm_cwd(VOLCF)), "wrapped in `dwm")
  expect_error(handler %>% estimate(dwm_cwd(CARBON)), "wrapped in `dwm")
  expect_error(
    handler %>% estimate(ratio(dwm_cwd(CARBON), cond())),
    "wrapped in `dwm"
  )

  DBI::dbExecute(con, "ALTER TABLE COND_DWM_CALC DROP COLUMN CWD_VOLCF_UNADJ")
  broken <- eval_handler(con, 1001, spec = dwm_analysis())
  expect_error(
    broken %>% aggregate(dwm(dwm_cwd(VOLCF))),
    "missing required DWM column.*CWD_VOLCF_UNADJ"
  )
})

test_that("DWM estimate() and aggregate() accept multiple scopes", {
  con <- setup_dwm_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, 1001, spec = dwm_analysis())

  est <- handler %>%
    estimate(dwm(dwm_cwd(CARBON)), cond()) %>%
    dplyr::collect()
  expect_setequal(est$var, c("dwm_cwd_CARBON", "prop"))

  agg <- handler %>%
    aggregate(dwm(dwm_cwd(VOLCF)), cond()) %>%
    dplyr::collect()
  expect_true(all(c("dwm_cwd_VOLCF", "prop") %in% colnames(agg)))
})

test_that("dwm() argument names control output names", {
  con <- setup_dwm_test_db()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))

  handler <- eval_handler(con, 1001, spec = dwm_analysis())

  est <- handler %>%
    estimate(dwm(
      coarse = dwm_cwd(DRYBIO),
      fine_small = dwm_fwd(DRYBIO, size = "SM")
    )) %>%
    dplyr::collect()
  expect_setequal(est$var, c("coarse", "fine_small"))

  agg <- handler %>%
    aggregate(dwm(coarse = dwm_cwd(DRYBIO))) %>%
    dplyr::collect()
  expect_true("coarse" %in% colnames(agg))
  expect_false("dwm_cwd_DRYBIO" %in% colnames(agg))
})
