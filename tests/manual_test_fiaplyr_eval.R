# Manual verification script for FiaplyrEval
# To be run with: source("tests/manual_test_fiaplyr_eval.R")
library(dplyr)
library(DBI)
library(duckdb)
devtools::load_all()

# Path to local DB
db_path <- "./db/fiadb_or.duckdb"
con <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)

handler <- eval_handler(con, 411001) |>
  mutate_tree(
    BA = DIA^2 * 0.005454
  ) |>
  mutate_cond(
    # round FORTYPCD to the nearest tens
    FORGRP = round(FORTYPCD, -1)
  ) |>
  set_cond_domains(FORGRP, OWNCD)

ps <- PostStratifiedEstimator(handler)

ps |>
  estimate(tree ~ VOLCFGRS | BA | DRYBIO_BOLE) |>
  collect()

handler@tree

ps |>
  estimate(cond ~ 1)
