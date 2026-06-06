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

trim_leading_blank_lines <- function(lines) {
  while (length(lines) && lines[1] == "") {
    lines <- lines[-1]
  }

  lines
}

build_one <- function(input) {
  output_name <- paste0(tools::file_path_sans_ext(basename(input)), ".md")
  output_path <- file.path(guide_dir, output_name)
  title <- extract_title(input)

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
  message("Built ", output_path)
}

invisible(lapply(qmd_files, build_one))