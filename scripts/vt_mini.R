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

as_bool <- function(x, default = TRUE) {
	if (is.null(x) || identical(x, "")) {
		return(default)
	}

	normalized <- tolower(trimws(x))
	if (normalized %in% c("true", "t", "1", "yes", "y")) {
		return(TRUE)
	}
	if (normalized %in% c("false", "f", "0", "no", "n")) {
		return(FALSE)
	}

	stop("Invalid boolean value: ", x)
}

resolve_project_root <- function() {
	script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)

	if (length(script_arg)) {
		script_path <- normalizePath(
			sub("^--file=", "", script_arg[1]),
			winslash = "/",
			mustWork = TRUE
		)
		return(normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE))
	}

	normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

ensure_schema_loaded <- function(project_root) {
	if (methods::isClass("StatusAnalysis") && methods::hasMethod("initialize_tables", "StatusAnalysis")) {
		return(invisible(NULL))
	}

	source(file.path(project_root, "R", "DatabaseMapping.R"), chdir = TRUE)
	source(file.path(project_root, "R", "AnalysisSchema.R"), chdir = TRUE)
	source(file.path(project_root, "R", "StatusAnalysis.R"), chdir = TRUE)
	source(file.path(project_root, "R", "GRMAnalysis.R"), chdir = TRUE)
}

materialize_subset <- function(sqlite_path, duckdb_path, evalid, overwrite = TRUE) {
	required_pkgs <- c("DBI", "RSQLite", "duckdb", "dplyr")
	missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

	if (length(missing_pkgs)) {
		stop("Missing required packages: ", paste(missing_pkgs, collapse = ", "))
	}

	# initialize_tables methods sourced from package files use `%>%`.
	suppressPackageStartupMessages(library(dplyr))

	if (!file.exists(sqlite_path)) {
		stop("SQLite database not found: ", sqlite_path)
	}

	if (file.exists(duckdb_path)) {
		if (overwrite) {
			unlink(duckdb_path, force = TRUE)
		} else {
			stop("DuckDB output already exists. Set --overwrite=true or choose a different --output path.")
		}
	}

	sqlite_con <- DBI::dbConnect(RSQLite::SQLite(), sqlite_path)
	on.exit(DBI::dbDisconnect(sqlite_con), add = TRUE)

	duck_con <- DBI::dbConnect(duckdb::duckdb(), dbdir = duckdb_path)
	on.exit({
		DBI::dbDisconnect(duck_con, shutdown = TRUE)
	}, add = TRUE)

	schema <- methods::new("StatusAnalysis")
	tables <- initialize_tables(schema, sqlite_con, evalid = evalid)

	eval_count <- tables$pop_eval %>%
		dplyr::summarise(n = dplyr::n()) %>%
		dplyr::collect()
	has_eval <- eval_count$n[[1]] > 0
	if (!has_eval) {
		stop("No POP_EVAL rows found for EVALID ", evalid)
	}

	output_name_map <- c(
		pop_eval = "POP_EVAL",
		pop_estn_unit = "POP_ESTN_UNIT",
		pop_stratum = "POP_STRATUM",
		pop_plot_stratum_assgn = "POP_PLOT_STRATUM_ASSGN",
		plot = "PLOT",
		cond = "COND",
		tree = "TREE",
		ref_species = "REF_SPECIES",
		subp_cond = "SUBP_COND"
	)

	for (nm in names(output_name_map)) {
		qry <- tables[[nm]]
		if (is.null(qry)) {
			message("Skipping ", output_name_map[[nm]], " (not available)")
			next
		}

		message("Writing ", output_name_map[[nm]], "...")
		data <- dplyr::collect(qry)
		DBI::dbWriteTable(duck_con, output_name_map[[nm]], data, overwrite = TRUE)
		message("  Rows: ", nrow(data))
	}

	message("Done. Wrote subset database to: ", duckdb_path)
}

main <- function() {
	project_root <- resolve_project_root()
	args <- parse_kv_args(commandArgs(trailingOnly = TRUE))

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

	evalid <- as.integer(args$evalid %||% "500601")
	if (is.na(evalid)) {
		stop("--evalid must be an integer value.")
	}

	overwrite <- as_bool(args$overwrite, default = TRUE)

	ensure_schema_loaded(project_root)
	materialize_subset(
		sqlite_path = sqlite_path,
		duckdb_path = duckdb_path,
		evalid = evalid,
		overwrite = overwrite
	)
}

`%||%` <- function(x, y) {
	if (is.null(x) || identical(x, "")) y else x
}

main()
