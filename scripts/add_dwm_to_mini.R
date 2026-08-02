#!/usr/bin/env Rscript

parse_kv_args <- function(args) {
  parsed <- list()

  for (arg in args) {
    if (!grepl("^--[^=]+=", arg)) {
      next
    }

    key <- sub("^--([^=]+)=.*$", "\\1", arg)
    value <- sub("^--[^=]+=(.*)$", "\\1", arg)
    parsed[[key]] <- value
  }

  parsed
}

`%||%` <- function(x, y) {
  if (is.null(x) || identical(x, "")) y else x
}

parse_evalid <- function(x) {
  if (is.null(x) || !nzchar(x)) {
    stop("Missing required --evalid=<integer> argument.", call. = FALSE)
  }

  evalid <- suppressWarnings(as.integer(x))
  if (is.na(evalid)) {
    stop("--evalid must be an integer.", call. = FALSE)
  }

  evalid
}

resolve_project_root <- function() {
  script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)

  if (length(script_arg)) {
    script_path <- normalizePath(
      sub("^--file=", "", script_arg[1]),
      winslash = "/",
      mustWork = TRUE
    )
    return(normalizePath(file.path(dirname(script_path), ".."), winslash = "/"))
  }

  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

copy_dwm_evaluation <- function(sqlite_path, duckdb_path, evalid, overwrite = FALSE) {
  required_pkgs <- c("DBI", "RSQLite", "duckdb")
  missing_pkgs <- required_pkgs[
    !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing_pkgs)) {
    stop("Missing required packages: ", paste(missing_pkgs, collapse = ", "), call. = FALSE)
  }

  if (!file.exists(sqlite_path)) {
    stop("SQLite database not found: ", sqlite_path, call. = FALSE)
  }
  if (!file.exists(duckdb_path)) {
    stop("DuckDB mini database not found: ", duckdb_path, call. = FALSE)
  }

  sqlite_con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_path)
  on.exit(DBI::dbDisconnect(sqlite_con), add = TRUE)

  if (!"COND_DWM_CALC" %in% DBI::dbListTables(sqlite_con)) {
    stop("SQLite database does not contain COND_DWM_CALC.", call. = FALSE)
  }

  source_fields <- DBI::dbListFields(sqlite_con, "COND_DWM_CALC")
  if (!"EVALID" %in% source_fields) {
    stop("COND_DWM_CALC must contain a direct EVALID column.", call. = FALSE)
  }

  dwm_data <- DBI::dbGetQuery(
    sqlite_con,
    "SELECT * FROM COND_DWM_CALC WHERE EVALID = ?",
    params = list(evalid)
  )
  if (!nrow(dwm_data)) {
    stop("No COND_DWM_CALC rows found for EVALID ", evalid, ".", call. = FALSE)
  }

  evaluation_queries <- list(
    POP_EVAL = "
      SELECT * FROM POP_EVAL WHERE EVALID = ?
    ",
    POP_ESTN_UNIT = "
      SELECT u.* FROM POP_ESTN_UNIT u
      JOIN POP_EVAL e ON e.CN = u.EVAL_CN
      WHERE e.EVALID = ?
    ",
    POP_STRATUM = "
      SELECT s.* FROM POP_STRATUM s
      JOIN POP_ESTN_UNIT u ON u.CN = s.ESTN_UNIT_CN
      JOIN POP_EVAL e ON e.CN = u.EVAL_CN
      WHERE e.EVALID = ?
    ",
    POP_PLOT_STRATUM_ASSGN = "
      SELECT a.* FROM POP_PLOT_STRATUM_ASSGN a
      JOIN POP_STRATUM s ON s.CN = a.STRATUM_CN
      JOIN POP_ESTN_UNIT u ON u.CN = s.ESTN_UNIT_CN
      JOIN POP_EVAL e ON e.CN = u.EVAL_CN
      WHERE e.EVALID = ?
    ",
    PLOT = "
      SELECT DISTINCT p.* FROM PLOT p
      JOIN POP_PLOT_STRATUM_ASSGN a ON a.PLT_CN = p.CN
      JOIN POP_STRATUM s ON s.CN = a.STRATUM_CN
      JOIN POP_ESTN_UNIT u ON u.CN = s.ESTN_UNIT_CN
      JOIN POP_EVAL e ON e.CN = u.EVAL_CN
      WHERE e.EVALID = ?
    ",
    COND = "
      SELECT DISTINCT c.* FROM COND c
      JOIN POP_PLOT_STRATUM_ASSGN a ON a.PLT_CN = c.PLT_CN
      JOIN POP_STRATUM s ON s.CN = a.STRATUM_CN
      JOIN POP_ESTN_UNIT u ON u.CN = s.ESTN_UNIT_CN
      JOIN POP_EVAL e ON e.CN = u.EVAL_CN
      WHERE e.EVALID = ?
    "
  )
  evaluation_data <- lapply(
    evaluation_queries,
    function(query) DBI::dbGetQuery(sqlite_con, query, params = list(evalid))
  )

  empty_tables <- names(evaluation_data)[
    vapply(evaluation_data, nrow, integer(1)) == 0L
  ]
  if (length(empty_tables)) {
    stop(
      "Evaluation ", evalid, " is missing required rows in: ",
      paste(empty_tables, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  duck_con <- DBI::dbConnect(duckdb::duckdb(), dbdir = duckdb_path)
  on.exit(DBI::dbDisconnect(duck_con, shutdown = TRUE), add = TRUE)

  if (DBI::dbExistsTable(duck_con, "COND_DWM_CALC") && !overwrite) {
    stop(
      "COND_DWM_CALC already exists. Use --overwrite=true to replace it.",
      call. = FALSE
    )
  }

  DBI::dbWriteTable(
    duck_con,
    "COND_DWM_CALC",
    dwm_data,
    overwrite = TRUE
  )

  for (table_name in names(evaluation_data)) {
    rows <- evaluation_data[[table_name]]
    existing_cn <- DBI::dbGetQuery(
      duck_con,
      paste0("SELECT CN FROM ", DBI::dbQuoteIdentifier(duck_con, table_name))
    )$CN
    rows <- rows[!rows$CN %in% existing_cn, , drop = FALSE]
    if (nrow(rows)) {
      DBI::dbAppendTable(duck_con, table_name, rows)
    }
  }

  message(
    "Wrote ", nrow(dwm_data), " COND_DWM_CALC rows and evaluation metadata for EVALID ",
    evalid, " to ", duckdb_path, "."
  )
}

main <- function() {
  args <- parse_kv_args(commandArgs(trailingOnly = TRUE))
  project_root <- resolve_project_root()

  sqlite_path <- normalizePath(
    args$input %||% file.path(project_root, "db", "SQLite_FIADB_VT.db"),
    winslash = "/",
    mustWork = FALSE
  )
  duckdb_path <- normalizePath(
    args$output %||% file.path(project_root, "db", "fiadb_vt_mini.duckdb"),
    winslash = "/",
    mustWork = FALSE
  )
  evalid <- parse_evalid(args$evalid)
  overwrite <- identical(tolower(args$overwrite %||% "false"), "true")

  copy_dwm_evaluation(
    sqlite_path = sqlite_path,
    duckdb_path = duckdb_path,
    evalid = evalid,
    overwrite = overwrite
  )
}

main()
