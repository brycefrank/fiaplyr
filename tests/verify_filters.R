# Verification script for filter_tree and filter_cond
library(dplyr)
library(DBI)
library(duckdb)
devtools::load_all()

# Connect to DB
db_path <- "./db/fiadb_or.duckdb"
if (!file.exists(db_path)) {
  stop("Database not found at ", db_path)
}
con <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)

evalid <- 411001

# 1. Baseline Count
handler <- eval_handler(con, evalid)
base_est <- PostStratifiedEstimator(handler) %>%
  estimate(tree(VOLCFGRS)) %>%
  collect()

# 2. Filter Tree (STATUSCD == 1)
handler_filtered <- eval_handler(con, evalid) %>%
  filter_tree(STATUSCD == 1, SPCD == 202) %>%
  set_cond_domains(FORTYPCD)

filt_est <- PostStratifiedEstimator(handler_filtered) %>%
  estimate(tree(VOLCFGRS)) %>%
  collect()
filt_tpa <- filt_est %>% dplyr::pull(VOLCFGRS)
print(paste("Filtered TPA (STATUSCD=1):", filt_tpa))

if (filt_tpa > base_tpa) {
  stop("Filtered TPA should be <= Base TPA")
}

# 3. Mutate then Filter Tree
handler_mut_filt <- eval_handler(con, evalid) %>%
  mutate_tree(is_live = ifelse(STATUSCD == 1, 1, 0)) %>%
  filter_tree(is_live == 1)

mut_filt_est <- PostStratifiedEstimator(handler_mut_filt) %>%
  estimate(tree(TPA_UNADJ)) %>%
  collect()
mut_filt_tpa <- mut_filt_est %>% dplyr::pull(TPA_UNADJ)
print(paste("Mutated & Filtered TPA (is_live=1):", mut_filt_tpa))

if (abs(filt_tpa - mut_filt_tpa) > 0.001) {
  stop("Filtering on mutated column gave different result than filtering directly")
}

# 4. Filter Cond
handler_cond <- eval_handler(con, evalid) %>%
  filter_cond(COND_STATUS_CD == 1) %>%
  set_cond_domains(FORTYPCD)

cond_est <- PostStratifiedEstimator(handler_cond) %>%
  estimate(cond()) %>%
  collect()
