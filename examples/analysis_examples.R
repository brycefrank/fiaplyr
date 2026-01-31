# Examples of Status and Change Analysis using the new Composition Pattern
library(fiaplyr)
library(dplyr)
library(dbplyr)
library(duckdb)

# Mock DB connection (replace with actual connection)
db <- DBI::dbConnect(duckdb::duckdb(), "./db/fiadb_or.duckdb")

explore_evals(db)

# -----------------------------------------------------------------------------
# Case 1: Status Analysis (Default)
# -----------------------------------------------------------------------------

# Initialize handler for a specific evaluation (e.g., EVALID 12345)
# By default, this uses the StatusAnalysis schema.
# It automatically loads PLOT, TREE, COND, etc.
status_handler <- eval_handler(db, evalid = 411001)

# Perform standard aggregation
# This delegates to StatusAnalysis::aggregate_data -> .make_tree_aggregates
status_agg <- status_handler %>%
  aggregate(tree ~ VOLCFGRS | 1 | VOLCFNET)

# -----------------------------------------------------------------------------
# Case 2: Change Analysis (Future Implementation)
# -----------------------------------------------------------------------------

# Initialize handler with the ChangeAnalysis schema.
# This would load change-specific tables (e.g., TREE_GRM_COMPONENT) in the future.
change_handler <- eval_handler(db, evalid = 411703, schema = new("ChangeAnalysis"))

# Perform change aggregation
# The API will support timepoint wrappers: b() for beginning, m() for midpoint, e() for ending.
# Note: This is currently a skeleton and will raise a "Not yet implemented" error.
tryCatch({
  change_agg <- change_handler %>%
    aggregate(tree ~ b(VOLCFGRS))
}, error = function(e) {
  message("Expected error (Change analysis not fully implemented): ", e$message)
})
