library(dplyr)
library(DBI)
library(duckdb)
library(ggplot2)
devtools::load_all()

# Path to local DB
db_path <- "./db/fiadb_or.duckdb"
con <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)

handler <- eval_handler(con, 411001) |>
  filter_tree(STATUSCD == 1) |>
  set_tree_domains(SPCD) |>
  set_cond_domains(COND_STATUS_CD, FORTYPCD)

handler |>
  aggregate(tree ~ VOLCFGRS | VOLCFNET)

ps <- PostStratifiedEstimator(handler)

ps |>
  estimate(tree ~ VOLCFGRS)
