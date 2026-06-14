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

parse_evalids <- function(x, default = c(500601L, 501103L)) {
	if (is.null(x) || identical(x, "")) {
		return(default)
	}

	parts <- unlist(strsplit(x, ",", fixed = TRUE), use.names = FALSE)
	parts <- trimws(parts)
	parts <- parts[nzchar(parts)]
	if (!length(parts)) {
		stop("--evalid/--evalids must contain at least one integer value.")
	}

	vals <- suppressWarnings(as.integer(parts))
	if (any(is.na(vals))) {
		stop("--evalid/--evalids must contain only integer values.")
	}

	unique(vals)
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
	invisible(project_root)
}

materialize_subset <- function(sqlite_path, duckdb_path, evalids, overwrite = TRUE) {
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

	tbl_names <- DBI::dbListTables(sqlite_con)
	resolve_table_name <- function(base_name) {
		exact <- tbl_names[tbl_names == base_name]
		if (length(exact)) {
			return(exact[[1]])
		}

		suff <- tbl_names[grepl(paste0("\\.", base_name, "$"), tbl_names)]
		if (length(suff)) {
			return(suff[[1]])
		}

		NULL
	}

	tbl <- function(base_name) {
		real_name <- resolve_table_name(base_name)
		if (is.null(real_name)) {
			return(NULL)
		}
		dplyr::tbl(sqlite_con, DBI::Id(table = real_name))
	}

	pop_eval_qry <- tbl("POP_EVAL")
	if (is.null(pop_eval_qry)) {
		stop("Missing required table: POP_EVAL")
	}

	pop_eval_qry <- pop_eval_qry %>%
		dplyr::filter(EVALID %in% !!evalids)

	found_evalids <- pop_eval_qry %>%
		dplyr::distinct(EVALID) %>%
		dplyr::collect() %>%
		dplyr::pull(EVALID)

	missing_evalids <- setdiff(evalids, found_evalids)
	if (length(missing_evalids)) {
		stop("Missing POP_EVAL rows for EVALID(s): ", paste(missing_evalids, collapse = ", "))
	}

	pop_estn_unit_qry <- tbl("POP_ESTN_UNIT")
	if (is.null(pop_estn_unit_qry)) {
		stop("Missing required table: POP_ESTN_UNIT")
	}
	pop_estn_unit_qry <- pop_estn_unit_qry %>%
		dplyr::semi_join(pop_eval_qry, by = c("EVAL_CN" = "CN"))

	pop_stratum_qry <- tbl("POP_STRATUM")
	if (is.null(pop_stratum_qry)) {
		stop("Missing required table: POP_STRATUM")
	}
	pop_stratum_qry <- pop_stratum_qry %>%
		dplyr::semi_join(pop_estn_unit_qry, by = c("ESTN_UNIT_CN" = "CN"))

	pop_plot_stratum_assgn_qry <- tbl("POP_PLOT_STRATUM_ASSGN")
	if (is.null(pop_plot_stratum_assgn_qry)) {
		stop("Missing required table: POP_PLOT_STRATUM_ASSGN")
	}
	pop_plot_stratum_assgn_qry <- pop_plot_stratum_assgn_qry %>%
		dplyr::semi_join(pop_stratum_qry, by = c("STRATUM_CN" = "CN"))

	plot_qry <- tbl("PLOT")
	if (is.null(plot_qry)) {
		stop("Missing required table: PLOT")
	}
	plot_qry <- plot_qry %>%
		dplyr::semi_join(pop_plot_stratum_assgn_qry, by = c("CN" = "PLT_CN"))

	grm_evalids <- evalids[grepl("03$", as.character(evalids))]
	has_grm_evalids <- length(grm_evalids) > 0
	if (!length(grm_evalids)) {
		grm_evalids <- evalids
		message("No EVALID ending in '03' requested; using all selected evals for TREE_GRM tables: ",
			paste(grm_evalids, collapse = ", "))
	} else {
		message("Using EVALID(s) for TREE_GRM tables: ", paste(grm_evalids, collapse = ", "))
	}

	grm_plot_qry <- plot_qry %>%
		dplyr::semi_join(
			pop_plot_stratum_assgn_qry %>%
				dplyr::filter(EVALID %in% !!grm_evalids) %>%
				dplyr::select(PLT_CN) %>%
				dplyr::distinct(),
			by = c("CN" = "PLT_CN")
		)

	if (has_grm_evalids) {
		message("Expanding 03 lineage in PLOT/COND/TREE using PREV_PLT_CN and PREV_TRE_CN links")

		prev_plot_refs_qry <- grm_plot_qry %>%
			dplyr::filter(!is.na(PREV_PLT_CN)) %>%
			dplyr::transmute(CN = PREV_PLT_CN) %>%
			dplyr::distinct()

		prev_plot_qry <- tbl("PLOT")
		if (!is.null(prev_plot_qry)) {
			prev_plot_qry <- prev_plot_qry %>%
				dplyr::semi_join(prev_plot_refs_qry, by = "CN")

			plot_qry <- dplyr::union(plot_qry, prev_plot_qry) %>%
				dplyr::distinct()
		}
	}

	plotgeom_qry <- tbl("PLOTGEOM")
	if (!is.null(plotgeom_qry)) {
		plotgeom_qry <- plotgeom_qry %>%
			dplyr::semi_join(plot_qry, by = "CN")
	}

	cond_qry <- tbl("COND")
	if (is.null(cond_qry)) {
		stop("Missing required table: COND")
	}
	cond_qry <- cond_qry %>%
		dplyr::semi_join(plot_qry, by = c("PLT_CN" = "CN"))

	tree_qry <- tbl("TREE")
	if (is.null(tree_qry)) {
		stop("Missing required table: TREE")
	}
	tree_qry <- tree_qry %>%
		dplyr::semi_join(cond_qry, by = c("PLT_CN", "CONDID"))

	if (has_grm_evalids) {
		prev_tree_refs_qry <- tree_qry %>%
			dplyr::filter(!is.na(PREV_TRE_CN)) %>%
			dplyr::transmute(CN = PREV_TRE_CN) %>%
			dplyr::distinct()

		prev_tree_qry <- tbl("TREE")
		if (!is.null(prev_tree_qry)) {
			prev_tree_qry <- prev_tree_qry %>%
				dplyr::semi_join(prev_tree_refs_qry, by = "CN")

			tree_qry <- dplyr::union(tree_qry, prev_tree_qry) %>%
				dplyr::distinct()
		}
	}

	tree_grm_begin_qry <- tbl("TREE_GRM_BEGIN")
	if (!is.null(tree_grm_begin_qry)) {
		tree_grm_begin_qry <- tree_grm_begin_qry %>%
			dplyr::semi_join(grm_plot_qry, by = c("PLT_CN" = "CN"))
	}

	tree_grm_midpt_qry <- tbl("TREE_GRM_MIDPT")
	if (!is.null(tree_grm_midpt_qry)) {
		tree_grm_midpt_qry <- tree_grm_midpt_qry %>%
			dplyr::semi_join(grm_plot_qry, by = c("PLT_CN" = "CN"))
	}

	tree_grm_component_qry <- tbl("TREE_GRM_COMPONENT")
	if (!is.null(tree_grm_component_qry)) {
		tree_grm_component_qry <- tree_grm_component_qry %>%
			dplyr::semi_join(grm_plot_qry, by = c("PLT_CN" = "CN"))
	}

	ref_species_qry <- tbl("REF_SPECIES")
	beginend_qry <- tbl("BEGINEND")

	output_name_map <- list(
		POP_EVAL = pop_eval_qry,
		POP_ESTN_UNIT = pop_estn_unit_qry,
		POP_STRATUM = pop_stratum_qry,
		POP_PLOT_STRATUM_ASSGN = pop_plot_stratum_assgn_qry,
		PLOT = plot_qry,
		PLOTGEOM = plotgeom_qry,
		COND = cond_qry,
		TREE = tree_qry,
		TREE_GRM_BEGIN = tree_grm_begin_qry,
		TREE_GRM_MIDPT = tree_grm_midpt_qry,
		TREE_GRM_COMPONENT = tree_grm_component_qry,
		REF_SPECIES = ref_species_qry,
		BEGINEND = beginend_qry
	)

	for (nm in names(output_name_map)) {
		qry <- output_name_map[[nm]]
		if (is.null(qry)) {
			message("Skipping ", nm, " (not available)")
			next
		}

		message("Writing ", nm, "...")
		data <- dplyr::collect(qry)
		DBI::dbWriteTable(duck_con, nm, data, overwrite = TRUE)
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

	evalid_arg <- args$evalids %||% args$evalid
	evalids <- parse_evalids(evalid_arg)

	overwrite <- as_bool(args$overwrite, default = TRUE)

	materialize_subset(
		sqlite_path = sqlite_path,
		duckdb_path = duckdb_path,
		evalids = evalids,
		overwrite = overwrite
	)
}

`%||%` <- function(x, y) {
	if (is.null(x) || identical(x, "")) y else x
}

main()
