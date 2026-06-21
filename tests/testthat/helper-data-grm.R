setup_grm_test_db <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  write_table <- function(name, data) {
    DBI::dbWriteTable(con, name, data)
  }

  # POP_EVAL
  write_table("POP_EVAL", data.frame(
    CN = 1,
    EVALID = 1001,
    EVAL_DESCR = "Test GRM Evaluation",
    stringsAsFactors = FALSE
  ))

  # POP_ESTN_UNIT
  write_table("POP_ESTN_UNIT", data.frame(
    CN = c(1, 2),
    EVAL_CN = c(1, 1),
    P1PNTCNT_EU = c(100, 200),
    AREA_USED = c(100, 200),
    stringsAsFactors = FALSE
  ))

  # POP_STRATUM
  write_table("POP_STRATUM", data.frame(
    CN = c(1, 2, 3, 4),
    ESTN_UNIT_CN = c(1, 1, 2, 2),
    P1POINTCNT = c(50, 50, 100, 100),
    P2POINTCNT = c(2, 2, 2, 2),
    ADJ_FACTOR_MICR = 1.0,
    ADJ_FACTOR_SUBP = 1.0,
    ADJ_FACTOR_MACR = 1.0,
    stringsAsFactors = FALSE
  ))

  # POP_PLOT_STRATUM_ASSGN
  write_table("POP_PLOT_STRATUM_ASSGN", data.frame(
    STRATUM_CN = c(1, 1, 2, 2, 3, 3, 4, 4),
    PLT_CN = c(101, 102, 103, 104, 201, 202, 203, 204),
    stringsAsFactors = FALSE
  ))

  # PLOT - with linked plot records (PREV_PLT_CN) for GRM
  write_table("PLOT", data.frame(
    CN = c(101, 102, 103, 104, 201, 202, 203, 204),
    INVYR = 2020,
    MEASYEAR = 2020,
    STATECD = 1,
    COUNTYCD = 1,
    PREV_PLT_CN = c(NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_),
    PLOT = c(1, 2, 3, 4, 5, 6, 7, 8),
    stringsAsFactors = FALSE
  ))

  # COND
  write_table("COND", data.frame(
    PLT_CN = c(101, 102, 103, 104, 104, 201, 202, 203, 204, 204),
    CONDID = c(1, 1, 1, 1, 2, 1, 1, 1, 1, 2),
    FORTYPCD = c(100, 100, 200, 200, 300, 100, 100, 200, 200, 300),
    OWNCD = c(1, 1, 2, 2, 2, 1, 1, 2, 2, 2),
    CONDPROP_UNADJ = c(1, 1, 1, 0.5, 0.5, 1, 1, 1, 0.5, 0.5),
    PROP_BASIS = "SUBP",
    COND_STATUS_CD = c(1, 1, 1, 2, 1, 1, 1, 1, 2, 1),
    stringsAsFactors = FALSE
  ))

  # TREE - with PREV_TRE_CN for GRM linkage
  write_table("TREE", data.frame(
    CN = c(1, 2, 3, 4, 5, 6, 7, 8),
    PLT_CN = c(101, 101, 103, 104, 201, 201, 203, 204),
    CONDID = c(1, 1, 1, 1, 1, 1, 1, 1),
    DIA = c(10, 12, 8, 14, 10, 12, 8, 14),
    HT = c(60, 70, 50, 80, 60, 70, 50, 80),
    VOLCFNET = c(10, 15, 5, 20, 10, 15, 5, 20),
    VOLCFGRS = c(11, 16, 6, 21, 11, 16, 6, 21),
    SPCD = c(1, 2, 1, 2, 1, 2, 1, 2),
    PREV_TRE_CN = c(NA_integer_, NA_integer_, NA_integer_, NA_integer_, 1, 2, 3, 4),
    TPA_UNADJ = c(6.0, 6.0, 6.0, 6.0, 6.0, 6.0, 6.0, 6.0),
    STATUSCD = c(1, 1, 1, 1, 1, 1, 1, 1),
    stringsAsFactors = FALSE
  ))

  # REF_SPECIES
  write_table("REF_SPECIES", data.frame(
    SPCD = c(1, 2),
    WOODLAND = c("N", "Y"),
    stringsAsFactors = FALSE
  ))

  # SUBP_COND
  write_table("SUBP_COND", data.frame(
    PLT_CN = c(101, 102, 103, 104, 104, 201, 202, 203, 204, 204),
    CONDID = c(1, 1, 1, 1, 2, 1, 1, 1, 1, 2),
    SUBP = 1,
    stringsAsFactors = FALSE
  ))

  # TREE_GRM_BEGIN - beginning of growth measurement period
  write_table("TREE_GRM_BEGIN", data.frame(
    TRE_CN = c(1, 2, 3, 4),
    STATUSCD = c(1, 1, 1, 1),
    DIA = c(10, 12, 8, 14),
    HT = c(60, 70, 50, 80),
    stringsAsFactors = FALSE
  ))

  # TREE_GRM_MIDPT - middle of growth measurement period
  write_table("TREE_GRM_MIDPT", data.frame(
    TRE_CN = c(1, 2, 3, 4),
    STATUSCD = c(1, 1, 1, 1),
    DIA = c(10.5, 12.3, 8.2, 14.5),
    HT = c(62, 72, 52, 82),
    stringsAsFactors = FALSE
  ))

  return(con)
}
