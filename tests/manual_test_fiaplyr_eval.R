library(dplyr)
library(DBI)
library(duckdb)
library(ggplot2)
devtools::load_all()

# Path to local DB
db_path <- "./db/fiadb_or.duckdb"
con <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)

handler <- eval_handler(con, 411001) |>
  subset(tree(STATUSCD == 1)) |>
  subset(cond(COND_STATUS_CD == 1)) |>
  partition(tree(SPCD))

handler |>
  aggregate(tree(VOLCFGRS, VOLCFNET))

psr <- PostStratifiedRatioEstimator(handler)

psr |>
  estimate_ratio(ratio(tree(VOLCFGRS), tree(VOLCFGRS)))
