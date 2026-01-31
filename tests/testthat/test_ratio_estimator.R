library(testthat)
library(dplyr)
library(DBI)
library(RSQLite)

# Setup Test DB
setup_test_db_sqlite <- function() {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")

  dbWriteTable(con, "POP_EVAL", data.frame(CN=1, EVALID=1, EVAL_DESCR="Test", stringsAsFactors=FALSE))
  dbWriteTable(con, "POP_ESTN_UNIT", data.frame(CN=1, EVAL_CN=1, P1PNTCNT_EU=100, stringsAsFactors=FALSE))
  dbWriteTable(con, "POP_STRATUM", data.frame(CN=1, ESTN_UNIT_CN=1, P1POINTCNT=100, P2POINTCNT=2, AREA_USED=1000, stringsAsFactors=FALSE))
  dbWriteTable(con, "POP_PLOT_STRATUM_ASSGN", data.frame(PLT_CN=c(101, 102), STRATUM_CN=c(1,1), stringsAsFactors=FALSE))
  dbWriteTable(con, "PLOT", data.frame(CN=c(101, 102), STATECD=1, INVYR=2000, PLOT=c(1,2), COUNTYCD=1, stringsAsFactors=FALSE))
  dbWriteTable(con, "COND", data.frame(PLT_CN=c(101, 102), CONDID=1, CONDPROP_UNADJ=1, stringsAsFactors=FALSE))

  # TREE: Plot 101 has 2 trees, Plot 102 has 1 tree.
  dbWriteTable(con, "TREE", data.frame(
    PLT_CN=c(101, 101, 102),
    CONDID=1,
    TPA_UNADJ=1,
    VOLCFNET=c(10, 20, 30), # Sum = 60. Plot101=30, Plot102=30.
    VOLCFGRS=c(11, 21, 31), # Sum = 63. Plot101=32, Plot102=31.
    BA=c(1, 2, 3),          # Sum = 6.  Plot101=3,  Plot102=3.
    stringsAsFactors=FALSE
  ))

  dbWriteTable(con, "SUBP_COND", data.frame(PLT_CN=c(101, 102), CONDID=1, SUBP=1, stringsAsFactors=FALSE))

  con
}

test_that("PostStratifiedRatioEstimator works", {
  con <- setup_test_db_sqlite()
  on.exit(dbDisconnect(con))

  handler <- eval_handler(con, 1)

  # Same handler for num and den
  est <- PostStratifiedRatioEstimator(handler, handler)

  # R = VOLCFNET / BA
  # Plot 101: VOL=30, BA=3. Ratio=10? No, Ratio of means.
  # Stratum 1 (Plots 101, 102). n_h=2.
  # Plot 101: VOL=30, BA=3.
  # Plot 102: VOL=30, BA=3.
  # Mean VOL = (30+30)/2 = 30.
  # Mean BA = (3+3)/2 = 3.
  # Ratio = 30 / 3 = 10.

  res <- estimate_ratio(est, tree ~ VOLCFNET / BA)

  expect_true("R_VOLCFNET_BA" %in% names(res))
  expect_equal(res$R_VOLCFNET_BA, 10)

  # Multiple ratios
  # R1 = VOLCFNET / BA
  # R2 = VOLCFGRS / BA
  # Mean VOLGRS = (32+31)/2 = 31.5.
  # Ratio2 = 31.5 / 3 = 10.5.

  res2 <- estimate_ratio(est, tree ~ VOLCFNET / BA | VOLCFGRS / BA)

  expect_true("R_VOLCFNET_BA" %in% names(res2))
  expect_true("R_VOLCFGRS_BA" %in% names(res2))
  expect_equal(res2$R_VOLCFNET_BA, 10)
  expect_equal(res2$R_VOLCFGRS_BA, 10.5)
})

test_that("parse_ratio_formula works", {
  f <- tree ~ VOL / BA | BIO / VOL
  p <- parse_ratio_formula(f)

  expect_equal(p$slot, "tree")
  expect_equal(length(p$ratios), 2)
  expect_equal(p$ratios[[1]]$numerator, "VOL")
  expect_equal(p$ratios[[1]]$denominator, "BA")
  expect_equal(p$ratios[[2]]$numerator, "BIO")
  expect_equal(p$ratios[[2]]$denominator, "VOL")
})
