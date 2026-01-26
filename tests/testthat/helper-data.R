setup_test_db <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  # POP_EVAL
  DBI::dbWriteTable(con, "POP_EVAL", data.frame(
    CN = 1,
    EVALID = 1001,
    EVAL_DESCR = "Test Evaluation",
    stringsAsFactors = FALSE
  ))

  # POP_ESTN_UNIT
  DBI::dbWriteTable(con, "POP_ESTN_UNIT", data.frame(
    CN = 1,
    EVAL_CN = 1,
    stringsAsFactors = FALSE
  ))

  # POP_STRATUM
  DBI::dbWriteTable(con, "POP_STRATUM", data.frame(
    CN = c(1, 2),
    ESTN_UNIT_CN = 1,
    stringsAsFactors = FALSE
  ))

  # POP_PLOT_STRATUM_ASSGN
  # Assign 2 plots to stratum 1 and 2 plots to stratum 2
  DBI::dbWriteTable(con, "POP_PLOT_STRATUM_ASSGN", data.frame(
    STRATUM_CN = c(1, 1, 2, 2),
    PLT_CN = c(101, 102, 103, 104),
    stringsAsFactors = FALSE
  ))

  # PLOT
  DBI::dbWriteTable(con, "PLOT", data.frame(
    CN = c(101, 102, 103, 104),
    INVYR = 2020,
    MEASYEAR = 2020,
    stringsAsFactors = FALSE
  ))

  # COND
  # Simple case: 1 condition per plot usually, maybe 2 on one plot
  DBI::dbWriteTable(con, "COND", data.frame(
    PLT_CN = c(101, 102, 103, 104, 104),
    CONDID = c(1, 1, 1, 1, 2),
    FORTYPCD = c(100, 100, 200, 200, 300),
    OWNCD = c(1, 1, 2, 2, 2),
    stringsAsFactors = FALSE
  ))

  # TREE
  # A few trees
  DBI::dbWriteTable(con, "TREE", data.frame(
    PLT_CN = c(101, 101, 103, 104),
    CONDID = c(1, 1, 1, 1),
    DIA = c(10, 12, 8, 14),
    VOLCFNET = c(10, 15, 5, 20),
    VOLCFGRS = c(11, 16, 6, 21),
    stringsAsFactors = FALSE
  ))

  # SUBP_COND
  # Minimal subp_cond table
  DBI::dbWriteTable(con, "SUBP_COND", data.frame(
    PLT_CN = c(101, 102, 103, 104, 104),
    CONDID = c(1, 1, 1, 1, 2),
    SUBP = 1,
    stringsAsFactors = FALSE
  ))

  return(con)
}
