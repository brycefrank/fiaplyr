#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (!length(script_arg)) {
  stop("Unable to determine script path from commandArgs().")
}

script_path <- normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE)
project_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
man_dir <- file.path(project_root, "man")
reference_dir <- file.path(project_root, "docs", "src", "content", "docs", "reference")

dir.create(reference_dir, recursive = TRUE, showWarnings = FALSE)

if (!dir.exists(man_dir)) {
  stop("Could not find man directory at: ", man_dir)
}

rd_files <- list.files(man_dir, pattern = "\\.Rd$", full.names = TRUE)
if (!length(rd_files)) {
  stop("No .Rd files found in ", man_dir)
}

trim <- function(x) {
  sub("^\\s+", "", sub("\\s+$", "", x))
}

compact_blank_lines <- function(lines) {
  if (!length(lines)) {
    return(lines)
  }

  out <- character()
  last_blank <- FALSE

  for (line in lines) {
    is_blank <- identical(line, "")
    if (is_blank && last_blank) {
      next
    }
    out <- c(out, line)
    last_blank <- is_blank
  }

  while (length(out) && out[1] == "") {
    out <- out[-1]
  }
  while (length(out) && out[length(out)] == "") {
    out <- out[-length(out)]
  }

  out
}

strip_backspaces <- function(line) {
  chars <- strsplit(line, "", fixed = TRUE)[[1]]
  out <- character()

  for (ch in chars) {
    if (identical(ch, "\b")) {
      if (length(out)) {
        out <- out[-length(out)]
      }
    } else {
      out <- c(out, ch)
    }
  }

  paste(out, collapse = "")
}

escape_yaml <- function(x) {
  x <- gsub('"', '\\"', x, fixed = TRUE)
  x
}

parse_namespace_exports <- function(path) {
  lines <- readLines(path, warn = FALSE)

  parse_values <- function(prefix) {
    pattern <- paste0("^", prefix, "\\((.+)\\)$")
    hits <- grep(pattern, lines, value = TRUE)
    if (!length(hits)) {
      return(character())
    }

    values <- sub(pattern, "\\1", hits)
    values <- gsub('"', "", values, fixed = TRUE)
    trim(values)
  }

  functions <- parse_values("export")
  classes <- parse_values("exportClasses")
  methods <- parse_values("exportMethods")

  list(functions = functions, classes = classes, methods = methods)
}

collect_aliases <- function(rd_path) {
  rd <- tools::parse_Rd(rd_path)
  alias_nodes <- rd[vapply(rd, function(node) identical(attr(node, "Rd_tag"), "\\alias"), logical(1))]

  aliases <- vapply(alias_nodes, function(node) {
    trim(paste(unlist(node), collapse = ""))
  }, character(1))

  aliases[aliases != ""]
}

rd_section_lines <- function(rd, rd_tag, heading) {
  idx <- which(vapply(rd, function(node) identical(attr(node, "Rd_tag"), rd_tag), logical(1)))[1]
  if (is.na(idx)) {
    return(character())
  }

  section <- rd[idx]
  class(section) <- "Rd"
  lines <- capture.output(tools::Rd2txt(section, out = "", fragment = TRUE))
  lines <- vapply(lines, strip_backspaces, character(1))
  lines <- compact_blank_lines(lines)

  header <- paste0(heading, ":")
  if (length(lines) && identical(trim(lines[1]), header)) {
    lines <- lines[-1]
  }

  lines <- gsub("^\\s{5}", "", lines)
  lines <- gsub("^\\s+$", "", lines)
  compact_blank_lines(lines)
}

rd_title <- function(rd, fallback) {
  idx <- which(vapply(rd, function(node) identical(attr(node, "Rd_tag"), "\\title"), logical(1)))[1]
  if (is.na(idx)) {
    return(fallback)
  }

  title <- trim(paste(unlist(rd[[idx]]), collapse = ""))
  if (!nzchar(title)) {
    fallback
  } else {
    title
  }
}

format_arguments <- function(lines) {
  if (!length(lines)) {
    return(character())
  }

  bullets <- character()
  current_name <- NULL
  current_desc <- character()

  flush_current <- function() {
    if (is.null(current_name)) {
      return(NULL)
    }

    desc <- paste(trim(current_desc), collapse = " ")
    desc <- gsub("\\s+", " ", desc)
    bullets <<- c(bullets, paste0("- `", current_name, "`: ", desc))
  }

  for (line in lines) {
    if (line == "") {
      next
    }

    if (identical(trim(line), "Arguments:")) {
      next
    }

    if (grepl("^[^:]+:", line)) {
      flush_current()
      parts <- strsplit(line, ":", fixed = TRUE)[[1]]
      current_name <- trim(parts[1])
      desc_first <- trim(paste(parts[-1], collapse = ":"))
      current_desc <- if (nzchar(desc_first)) c(desc_first) else character()
    } else if (!is.null(current_name)) {
      current_desc <- c(current_desc, line)
    }
  }

  flush_current()
  bullets
}

markdown_for_rd <- function(rd_path) {
  rd <- tools::parse_Rd(rd_path)
  base <- tools::file_path_sans_ext(basename(rd_path))
  fallback_title <- tools::toTitleCase(gsub("_", " ", gsub("-class$", " Class", base)))
  title <- rd_title(rd, fallback_title)

  sections <- list(
    Description = rd_section_lines(rd, "\\description", "Description"),
    Usage = rd_section_lines(rd, "\\usage", "Usage"),
    Arguments = rd_section_lines(rd, "\\arguments", "Arguments"),
    Value = rd_section_lines(rd, "\\value", "Value"),
    Slots = rd_section_lines(rd, "\\section", "Section"),
    Examples = rd_section_lines(rd, "\\examples", "Examples")
  )

  description <- sections$Description

  description_line <- if (length(description)) {
    gsub("\\s+", " ", paste(description, collapse = " "))
  } else {
    paste0("Reference for `", base, "`.")
  }

  lines <- c(
    "---",
    paste0('title: "', escape_yaml(title), '"'),
    paste0('description: "', escape_yaml(description_line), '"'),
    "---",
    "",
    "Auto-generated from `man/` Rd files. Do not edit this page by hand.",
    ""
  )

  if (length(sections$Description)) {
    lines <- c(lines, "## Description", "", sections$Description, "")
  }

  if (length(sections$Usage)) {
    lines <- c(lines, "## Usage", "", "```r", sections$Usage, "```", "")
  }

  if (length(sections$Arguments)) {
    args_md <- format_arguments(sections$Arguments)
    if (length(args_md)) {
      lines <- c(lines, "## Arguments", "", args_md, "")
    }
  }

  if (length(sections$Value)) {
    lines <- c(lines, "## Value", "", sections$Value, "")
  }

  if (length(sections$Slots)) {
    lines <- c(lines, "## Additional Details", "", sections$Slots, "")
  }

  if (length(sections$Examples)) {
    lines <- c(lines, "## Examples", "", "```r", sections$Examples, "```", "")
  }

  compact_blank_lines(lines)
}

exports <- parse_namespace_exports(file.path(project_root, "NAMESPACE"))

slug_from_symbol <- function(symbol) {
  if (identical(symbol, "evalid<-")) {
    return("evalid-set")
  }

  slug <- gsub("[^[:alnum:]_-]+", "-", symbol)
  gsub("-+", "-", slug)
}

has_alias_for_symbol <- function(aliases, symbol) {
  any(aliases == symbol | startsWith(aliases, paste0(symbol, ",")))
}

rd_meta <- lapply(rd_files, function(path) {
  base <- tools::file_path_sans_ext(basename(path))
  aliases <- tryCatch(collect_aliases(path), error = function(e) character())
  list(path = path, base = base, aliases = aliases)
})

pick_rd_for_function <- function(symbol) {
  by_base <- which(vapply(rd_meta, function(x) identical(x$base, symbol), logical(1)))
  if (length(by_base)) {
    return(rd_meta[[by_base[1]]]$path)
  }

  by_alias <- which(vapply(rd_meta, function(x) has_alias_for_symbol(x$aliases, symbol), logical(1)))
  if (length(by_alias)) {
    return(rd_meta[[by_alias[1]]]$path)
  }

  NULL
}

pick_rd_for_class <- function(class_name) {
  class_topic <- paste0(class_name, "-class")

  by_base <- which(vapply(rd_meta, function(x) identical(x$base, class_topic), logical(1)))
  if (length(by_base)) {
    return(rd_meta[[by_base[1]]]$path)
  }

  by_alias <- which(vapply(rd_meta, function(x) {
    has_alias_for_symbol(x$aliases, class_topic) || has_alias_for_symbol(x$aliases, class_name)
  }, logical(1)))
  if (length(by_alias)) {
    return(rd_meta[[by_alias[1]]]$path)
  }

  NULL
}

function_topics <- list()
missing_functions <- character()

function_symbols <- unique(c(exports$functions, exports$methods))

for (symbol in function_symbols) {
  rd_path <- pick_rd_for_function(symbol)
  if (is.null(rd_path)) {
    missing_functions <- c(missing_functions, symbol)
    next
  }

  function_topics[[length(function_topics) + 1]] <- list(
    symbol = symbol,
    slug = slug_from_symbol(symbol),
    rd_path = rd_path
  )
}

class_topics <- list()
missing_classes <- character()

for (class_name in unique(exports$classes)) {
  rd_path <- pick_rd_for_class(class_name)
  if (is.null(rd_path)) {
    missing_classes <- c(missing_classes, class_name)
    next
  }

  class_topics[[length(class_topics) + 1]] <- list(
    symbol = class_name,
    slug = paste0(class_name, "-class"),
    rd_path = rd_path
  )
}

if (!length(function_topics) && !length(class_topics)) {
  stop("No exported Rd topics were selected for reference generation.")
}

existing_md <- list.files(reference_dir, pattern = "\\.md$", full.names = TRUE)
if (length(existing_md)) {
  unlink(existing_md, force = TRUE)
}

for (topic in c(function_topics, class_topics)) {
  md_path <- file.path(reference_dir, paste0(topic$slug, ".md"))
  md <- markdown_for_rd(topic$rd_path)
  writeLines(md, md_path)
  message("Built ", md_path)
}

index_lines <- c(
  "---",
  "title: \"R Reference\"",
  "description: \"Auto-generated reference pages from fiaplyr Rd documentation.\"",
  "---",
  "",
  "These pages are generated from `man/*.Rd` during the docs build.",
  ""
)

if (length(function_topics)) {
  index_lines <- c(index_lines, "## Functions", "")
  function_links <- vapply(function_topics, function(topic) {
    paste0("- [`", topic$symbol, "`](./", topic$slug, ")")
  }, character(1))
  index_lines <- c(index_lines, function_links, "")
}

if (length(class_topics)) {
  index_lines <- c(index_lines, "## Classes", "")
  class_links <- vapply(class_topics, function(topic) {
    paste0("- [`", topic$symbol, "`](./", topic$slug, ")")
  }, character(1))
  index_lines <- c(index_lines, class_links, "")
}

writeLines(index_lines, file.path(reference_dir, "index.md"))
message("Built ", file.path(reference_dir, "index.md"))

if (length(missing_functions) || length(missing_classes)) {
  warning(
    "Some exported topics were not found in man/. Missing functions: ",
    paste(missing_functions, collapse = ", "),
    "; missing classes: ",
    paste(missing_classes, collapse = ", ")
  )
}
