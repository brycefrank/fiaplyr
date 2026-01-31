# Manual verification script for FiaplyrEval
# To be run with: source("tests/manual_test_fiaplyr_eval.R")
library(dplyr)
library(DBI)
library(duckdb)
devtools::load_all()

# Path to local DB
db_path <- "./db/fiadb_or.duckdb"
con <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)

handler_a <- eval_handler(con, 411001) |>
  set_cond_domains(OWNCD)

handler_b <- eval_handler(con, 411001) |>
  set_cond_domains(COND_STATUS_CD)

psr <- PostStratifiedRatioEstimator(handler_a, handler_b)

psr |>
  estimate_ratio(tree ~ VOLCFGRS,  tree ~ VOLCFGRS)
