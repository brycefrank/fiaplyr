eval_macro_vec <- function(macro, data) {
  as.numeric(rlang::eval_tidy(macro$expr, data = data))
}

test_that("rvalidator snum metadata covers derivative GRM metrics", {
  skip_if_not_installed("rvalidator")

  snum_defs <- unique(rvalidator::parameters("snum")[, c("ATTRIBUTE_NBR", "ATTRIBUTE_DESCR")])
  desc <- snum_defs$ATTRIBUTE_DESCR

  expect_true(any(grepl("gross growth", desc, ignore.case = TRUE)))
  expect_true(any(grepl("net growth", desc, ignore.case = TRUE)))
  expect_true(any(grepl("net change", desc, ignore.case = TRUE)))

  # We currently validate these via macro identities because direct snum labels
  # are not exposed with these exact terms.
  expect_false(any(grepl("gross ingrowth", desc, ignore.case = TRUE)))
  expect_false(any(grepl("\\baccretion\\b", desc, ignore.case = TRUE)))
})

test_that("grm_gross_ingrowth equals growth-ingrowth plus growth-reversion", {
  dat <- data.frame(
    transition = c("ingrowth", "reversion1", "reversion2", "survivor", "other"),
    TPA_UNADJ = c(10, 11, 12, 13, 14),
    TPA_UNADJ_begin = c(20, 21, 22, 23, 24),
    REMPER = c(4, 4, 4, 4, 4)
  )

  gross_ingrowth <- eval_macro_vec(grm_gross_ingrowth(1, annualize = TRUE, adjust = "none"), dat)
  ingrowth <- eval_macro_vec(grm_growth_ingrowth(1, annualize = TRUE, adjust = "none"), dat)
  reversion <- eval_macro_vec(grm_growth_reversion(1, annualize = TRUE, adjust = "none"), dat)

  expect_equal(gross_ingrowth, ingrowth + reversion)
})

test_that("grm_accretion equals GS + GI + GR + GM + GC + GD", {
  dat <- data.frame(
    transition = c("survivor", "ingrowth", "reversion2", "mortality1", "cut1", "diversion2", "other"),
    TPA_UNADJ = c(10, 11, 12, 13, 14, 15, 16),
    TPA_UNADJ_begin = c(20, 21, 22, 23, 24, 25, 26),
    REMPER = c(4, 4, 4, 4, 4, 4, 4)
  )

  accretion <- eval_macro_vec(grm_accretion(1, annualize = TRUE, adjust = "none"), dat)
  survivor <- eval_macro_vec(grm_growth_survivor(1, annualize = TRUE, adjust = "none"), dat)
  ingrowth <- eval_macro_vec(grm_growth_ingrowth(1, annualize = TRUE, adjust = "none"), dat)
  reversion <- eval_macro_vec(grm_growth_reversion(1, annualize = TRUE, adjust = "none"), dat)
  mortality <- eval_macro_vec(grm_growth_mortality(1, annualize = TRUE, adjust = "none"), dat)
  cut <- eval_macro_vec(grm_growth_cut(1, annualize = TRUE, adjust = "none"), dat)
  diversion <- eval_macro_vec(grm_growth_diversion(1, annualize = TRUE, adjust = "none"), dat)

  expect_equal(accretion, survivor + ingrowth + reversion + mortality + cut + diversion)
})

test_that("grm_gross_growth equals gross ingrowth plus accretion", {
  dat <- data.frame(
    transition = c("survivor", "ingrowth", "reversion1", "mortality2", "cut2", "other"),
    TPA_UNADJ = c(10, 11, 12, 13, 14, 15),
    TPA_UNADJ_begin = c(20, 21, 22, 23, 24, 25),
    REMPER = c(4, 4, 4, 4, 4, 4)
  )

  gross_growth <- eval_macro_vec(grm_gross_growth(1, annualize = TRUE, adjust = "none"), dat)
  gross_ingrowth <- eval_macro_vec(grm_gross_ingrowth(1, annualize = TRUE, adjust = "none"), dat)
  accretion <- eval_macro_vec(grm_accretion(1, annualize = TRUE, adjust = "none"), dat)

  expect_equal(gross_growth, gross_ingrowth + accretion)
})

test_that("grm_net_growth equals gross growth minus mortality", {
  dat <- data.frame(
    transition = c("survivor", "ingrowth", "reversion2", "mortality0", "cut1", "other"),
    TPA_UNADJ = c(10, 11, 12, 13, 14, 15),
    TPA_UNADJ_begin = c(20, 21, 22, 23, 24, 25),
    REMPER = c(4, 4, 4, 4, 4, 4)
  )

  net_growth <- eval_macro_vec(grm_net_growth(1, annualize = TRUE, adjust = "none"), dat)
  gross_growth <- eval_macro_vec(grm_gross_growth(1, annualize = TRUE, adjust = "none"), dat)
  mortality <- eval_macro_vec(grm_mortality(1, annualize = TRUE, adjust = "none"), dat)

  expect_equal(net_growth, gross_growth - mortality)
})

test_that("grm_net_change equals net growth minus removals", {
  dat <- data.frame(
    transition = c("survivor", "ingrowth", "reversion1", "mortality1", "cut2", "diversion1", "other"),
    TPA_UNADJ = c(10, 11, 12, 13, 14, 15, 16),
    TPA_UNADJ_begin = c(20, 21, 22, 23, 24, 25, 26),
    REMPER = c(4, 4, 4, 4, 4, 4, 4)
  )

  net_change <- eval_macro_vec(grm_net_change(1, annualize = TRUE, adjust = "none"), dat)
  net_growth <- eval_macro_vec(grm_net_growth(1, annualize = TRUE, adjust = "none"), dat)
  removals <- eval_macro_vec(grm_removals(1, annualize = TRUE, adjust = "none"), dat)

  expect_equal(net_change, net_growth - removals)
})
