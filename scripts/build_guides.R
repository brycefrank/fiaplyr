#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (!length(script_arg)) {
  stop("Unable to determine script path from commandArgs().")
}

script_path <- normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE)
project_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
vignette_dir <- file.path(project_root, "vignettes")
guide_dir <- file.path(project_root, "docs", "src", "content", "docs", "guides")

dir.create(guide_dir, recursive = TRUE, showWarnings = FALSE)

qmd_files <- list.files(
  vignette_dir,
  pattern = "\\.qmd$",
  full.names = TRUE
)

if (!length(qmd_files)) {
  stop("No .qmd vignette files found in ", vignette_dir)
}

if (Sys.which("quarto") == "") {
  stop("Quarto CLI not found in PATH. Install Quarto to build guides.")
}

split_frontmatter <- function(lines) {
  if (length(lines) < 3 || lines[1] != "---") {
    return(list(frontmatter = character(), body = lines))
  }

  end_frontmatter <- which(lines[-1] == "---")[1]
  if (is.na(end_frontmatter)) {
    return(list(frontmatter = character(), body = lines))
  }

  list(
    frontmatter = lines[2:end_frontmatter],
    body = lines[-seq_len(end_frontmatter + 1)]
  )
}

extract_title <- function(path) {
  lines <- readLines(path, warn = FALSE)
  parts <- split_frontmatter(lines)

  if (!length(parts$frontmatter)) {
    return(tools::toTitleCase(gsub("_", " ", tools::file_path_sans_ext(basename(path)))))
  }

  title_line <- grep("^title\\s*:", parts$frontmatter, value = TRUE)

  if (!length(title_line)) {
    return(tools::toTitleCase(gsub("_", " ", tools::file_path_sans_ext(basename(path)))))
  }

  title <- sub("^title\\s*:\\s*", "", title_line[1])
  title <- sub('^"', "", title)
  title <- sub('"$', "", title)
  title <- sub("^'", "", title)
  title <- sub("'$", "", title)
  title
}

rewrite_links <- function(lines) {
  gsub("\\(([^)#]+)\\.qmd(#[^)]+)?\\)", "(\\1\\2)", lines, perl = TRUE)
}

rewrite_chunk_fences <- function(lines) {
  lines <- gsub("^```\\{([[:alnum:]_+-]+)[^}]*\\}$", "```\\1", lines, perl = TRUE)
  lines <- gsub("^```\\{[^}]*\\}$", "```", lines, perl = TRUE)
  lines
}

strip_title_heading <- function(lines, title) {
  if (!length(lines)) {
    return(lines)
  }

  first_non_blank <- which(lines != "")[1]
  if (is.na(first_non_blank)) {
    return(lines)
  }

  heading <- lines[first_non_blank]
  expected <- paste0("# ", title)

  if (identical(heading, expected)) {
    lines <- lines[-seq_len(first_non_blank)]
    lines <- trim_leading_blank_lines(lines)
  }

  lines
}

trim_leading_blank_lines <- function(lines) {
  while (length(lines) && lines[1] == "") {
    lines <- lines[-1]
  }

  lines
}

build_from_source <- function(input, title, output_path) {
  lines <- readLines(input, warn = FALSE)
  parts <- split_frontmatter(lines)
  body <- trim_leading_blank_lines(parts$body)
  body <- rewrite_chunk_fences(body)
  body <- rewrite_links(body)

  final <- c(
    "---",
    paste0('title: "', title, '"'),
    "---",
    "",
    body
  )

  writeLines(final, output_path)
}

build_with_quarto <- function(input, title, output_path) {
  output_name <- paste0(tools::file_path_sans_ext(basename(input)), ".md")
  rendered_path <- file.path(dirname(input), output_name)
  rendered_assets <- file.path(
    dirname(input),
    paste0(tools::file_path_sans_ext(output_name), "_files")
  )
  profile_path <- tempfile("fiaplyr-quarto-profile-", fileext = ".R")

  profile_lines <- c(
    "if (requireNamespace(\"pkgload\", quietly = TRUE)) {",
    sprintf("  pkgload::load_all(\"%s\", quiet = TRUE)", project_root),
    "} else if (requireNamespace(\"devtools\", quietly = TRUE)) {",
    sprintf("  devtools::load_all(\"%s\", quiet = TRUE)", project_root),
    "}"
  )

  writeLines(profile_lines, profile_path)
  on.exit(unlink(profile_path, force = TRUE), add = TRUE)

  args <- c(
    "render",
    input,
    "--to", "gfm"
  )

  render_output <- system2(
    "quarto",
    args,
    stdout = TRUE,
    stderr = TRUE,
    env = c(paste0("R_PROFILE_USER=", profile_path))
  )

  status <- attr(render_output, "status")
  if (!is.null(status) && status != 0) {
    warning(
      "Failed to render ", basename(input), " with Quarto; falling back to source conversion.\n",
      paste(render_output, collapse = "\n")
    )
    return(FALSE)
  }

  if (!file.exists(rendered_path)) {
    warning(
      "Quarto did not produce expected output file for ", basename(input),
      "; falling back to source conversion. Expected: ", rendered_path
    )
    return(FALSE)
  }

  if (dir.exists(rendered_assets)) {
    output_assets <- file.path(guide_dir, basename(rendered_assets))
    unlink(output_assets, recursive = TRUE, force = TRUE)
    file.copy(rendered_assets, output_assets, recursive = TRUE)
  }

  body <- readLines(rendered_path, warn = FALSE)
  body <- strip_title_heading(body, title)
  body <- rewrite_links(body)
  body <- trim_leading_blank_lines(body)

  final <- c(
    "---",
    paste0('title: "', title, '"'),
    "---",
    "",
    body
  )

  writeLines(final, output_path)
  unlink(rendered_path, force = TRUE)
  if (dir.exists(rendered_assets)) {
    unlink(rendered_assets, recursive = TRUE, force = TRUE)
  }
  TRUE
}

build_one <- function(input) {
  output_name <- paste0(tools::file_path_sans_ext(basename(input)), ".md")
  output_path <- file.path(guide_dir, output_name)
  title <- extract_title(input)

  rendered <- build_with_quarto(input, title, output_path)

  if (!rendered) {
    build_from_source(input, title, output_path)
  }

  message("Built ", output_path)
}

normalize_readme_for_guide <- function(lines) {
  # Drop the README title/logo header and badge comments for docs rendering.
  title_line <- grep("^#\\s+", lines)[1]
  if (!is.na(title_line)) {
    lines <- lines[-seq_len(title_line)]
  }

  lines <- trim_leading_blank_lines(lines)
  lines <- lines[!grepl("^<!-- badges:", lines)]
  lines <- trim_leading_blank_lines(lines)
  rewrite_links(lines)
}

build_readme_if_possible <- function() {
  readme_rmd_path <- file.path(project_root, "README.Rmd")

  if (!file.exists(readme_rmd_path)) {
    warning("README.Rmd not found at ", readme_rmd_path, "; skipping README rebuild")
    return(invisible(NULL))
  }

  if (!requireNamespace("devtools", quietly = TRUE)) {
    warning("Package 'devtools' not installed; skipping README rebuild from README.Rmd")
    return(invisible(NULL))
  }

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(project_root)

  devtools::build_readme()
}

build_getting_started_from_readme <- function() {
  readme_path <- file.path(project_root, "README.md")
  output_path <- file.path(guide_dir, "getting_started.md")

  if (!file.exists(readme_path)) {
    warning("README.md not found at ", readme_path, "; skipping getting_started sync")
    return(invisible(NULL))
  }

  body <- readLines(readme_path, warn = FALSE)
  body <- normalize_readme_for_guide(body)

  final <- c(
    "---",
    'title: "Getting Started"',
    "---",
    "",
    "<!-- Generated from README.Rmd via README.md by scripts/build_guides.R. Edit README.Rmd only. -->",
    "",
    body
  )

  writeLines(final, output_path)
  message("Built ", output_path, " from README.md")
}

invisible(lapply(qmd_files, build_one))
build_readme_if_possible()
build_getting_started_from_readme()