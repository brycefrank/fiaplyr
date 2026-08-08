setup_grm_test_db <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  write_table <- function(name, data) {
    DBI::dbWriteTable(con, name, data)
  }

  # POP_EVAL
  write_table("POP_EVAL", data.frame(
    CN = 1,
    EVALID = 1003,
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

  # POP_PLOT_STRATUM_ASSGN - only assigned to visit 2 (remeasured) plots
  write_table("POP_PLOT_STRATUM_ASSGN", data.frame(
    STRATUM_CN = c(1, 1, 2, 2, 3, 3, 4, 4),
    PLT_CN = c(1001, 1002, 1003, 1004, 1201, 1202, 1203, 1204),
    stringsAsFactors = FALSE
  ))

  # PLOT - plots visited twice (visit 1 and visit 2)
  # Visit 1: CN = 101-108, PREV_PLT_CN = NA
  # Visit 2: CN = 1001-1008, PREV_PLT_CN = 101-108
  write_table("PLOT", data.frame(
    CN = c(101, 102, 103, 104, 201, 202, 203, 204, 1001, 1002, 1003, 1004, 1201, 1202, 1203, 1204),
    INVYR = c(2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2024, 2024, 2024, 2024, 2024, 2024, 2024, 2024),
    MEASYEAR = c(2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2024, 2024, 2024, 2024, 2024, 2024, 2024, 2024),
    STATECD = 1,
    COUNTYCD = 1,
    PREV_PLT_CN = c(NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, 101L, 102L, 103L, 104L, 201L, 202L, 203L, 204L),
    PLOT = c(1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7, 8),
    stringsAsFactors = FALSE
  ))

  # COND - both visit 1 and visit 2
  write_table("COND", data.frame(
    PLT_CN = c(101, 102, 103, 104, 104, 201, 202, 203, 204, 204, 1001, 1002, 1003, 1004, 1004, 1201, 1202, 1203, 1204, 1204),
    CONDID = c(1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2),
    FORTYPCD = c(100, 100, 200, 200, 300, 100, 100, 200, 200, 300, 100, 100, 200, 200, 300, 100, 100, 200, 200, 300),
    OWNCD = c(1, 1, 2, 2, 2, 1, 1, 2, 2, 2, 1, 1, 2, 2, 2, 1, 1, 2, 2, 2),
    CONDPROP_UNADJ = c(1, 1, 1, 0.5, 0.5, 1, 1, 1, 0.5, 0.5, 1, 1, 1, 0.5, 0.5, 1, 1, 1, 0.5, 0.5),
    PROP_BASIS = "SUBP",
    COND_STATUS_CD = c(1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1),
    stringsAsFactors = FALSE
  ))

  # TREE - trees measured at visit 1 and visit 2
  # Visit 1: CN = 1-8
  # Visit 2: CN = 101-108, PREV_TRE_CN = 1-8
  write_table("TREE", data.frame(
    CN = c(1, 2, 3, 4, 5, 6, 7, 8, 101, 102, 103, 104, 105, 106, 107, 108),
    PLT_CN = c(101, 101, 103, 104, 201, 201, 203, 204, 1001, 1001, 1003, 1004, 1201, 1201, 1203, 1204),
    CONDID = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    DIA = c(10, 12, 8, 14, 10, 12, 8, 14, 11, 13, 9, 15, 11, 13, 9, 15),
    HT = c(60, 70, 50, 80, 60, 70, 50, 80, 62, 72, 52, 82, 62, 72, 52, 82),
    VOLCFNET = c(10, 15, 5, 20, 10, 15, 5, 20, 11, 16, 6, 21, 11, 16, 6, 21),
    VOLCFGRS = c(11, 16, 6, 21, 11, 16, 6, 21, 12, 17, 7, 22, 12, 17, 7, 22),
    SPCD = c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2),
    PREV_TRE_CN = c(NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L),
    REMPER = c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4),
    TPA_UNADJ = c(6.0, 6.0, 6.0, 6.0, 6.0, 6.0, 6.0, 6.0, 6.0, 6.0, 6.0, 6.0, 6.0, 6.0, 6.0, 6.0),
    STATUSCD = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    stringsAsFactors = FALSE
  ))

  # REF_SPECIES
  write_table("REF_SPECIES", data.frame(
    SPCD = c(1, 2),
    WOODLAND = c("N", "Y"),
    stringsAsFactors = FALSE
  ))

  # SUBP_COND - for both visit 1 and visit 2
  write_table("SUBP_COND", data.frame(
    PLT_CN = c(101, 102, 103, 104, 104, 201, 202, 203, 204, 204, 1001, 1002, 1003, 1004, 1004, 1201, 1202, 1203, 1204, 1204),
    CONDID = c(1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2),
    SUBP = 1,
    stringsAsFactors = FALSE
  ))

  # TREE_GRM_BEGIN - beginning measurements for visit 2 trees
  # (attributes carried forward from the visit 1 measurement of the same tree)
  write_table("TREE_GRM_BEGIN", data.frame(
    TRE_CN = c(101, 102, 103, 104, 105, 106, 107, 108),
    STATUSCD = c(1, 1, 1, 1, 1, 1, 1, 1),
    DIA = c(10, 12, 8, 14, 10, 12, 8, 14),
    HT = c(60, 70, 50, 80, 60, 70, 50, 80),
    stringsAsFactors = FALSE
  ))

  # TREE_GRM_MIDPT - middle measurements for visit 2 trees
  write_table("TREE_GRM_MIDPT", data.frame(
    TRE_CN = c(101, 102, 103, 104, 105, 106, 107, 108),
    STATUSCD = c(1, 1, 1, 1, 1, 1, 1, 1),
    DIA = c(10.5, 12.3, 8.2, 14.5, 10.5, 12.3, 8.2, 14.5),
    HT = c(62, 72, 52, 82, 62, 72, 52, 82),
    VOLCFNET = c(10.5, 15.5, 5.5, 20.5, 10.5, 15.5, 5.5, 20.5),
    VOLCFGRS = c(11.5, 16.5, 6.5, 21.5, 11.5, 16.5, 6.5, 21.5),
    stringsAsFactors = FALSE
  ))

  # TREE_GRM_COMPONENT - basis-specific GRM component/subtype columns
  write_table("TREE_GRM_COMPONENT", data.frame(
    TRE_CN = c(101, 102, 103, 104, 105, 106, 107, 108),
    SUBP_SUBPTYP_GRM_AL_FOREST = c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 9L),
    SUBP_COMPONENT_AL_FOREST = c(
      "MORTALITY1", "MORTALITY1", "MORTALITY1", "CUT1",
      "MORTALITY1", "MORTALITY1", "CUT1", "MORTALITY1"
    ),
    SUBP_SUBPTYP_GRM_GS_TIMBER = c(3L, 3L, 3L, 3L, 2L, 2L, 2L, 2L),
    SUBP_COMPONENT_GS_TIMBER = c(
      "CUT1", "CUT1", "CUT1", "CUT1",
      "MORTALITY1", "MORTALITY1", "MORTALITY1", "MORTALITY1"
    ),
    SUBP_SUBPTYP_GRM_SL_TIMBER = c(2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L),
    SUBP_COMPONENT_SL_TIMBER = c(
      "MORTALITY1", "MORTALITY1", "MORTALITY1", "MORTALITY1",
      "CUT1", "CUT1", "CUT1", "CUT1"
    ),
    stringsAsFactors = FALSE
  ))

  return(con)
}
