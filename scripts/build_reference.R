#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (!length(script_arg)) {
  stop("Unable to determine script path from commandArgs().")
}

script_path <- normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE)
project_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
man_dir <- file.path(project_root, "man")
reference_dir <- file.path(project_root, "docs", "src", "content", "docs", "reference")
guides_dir <- file.path(project_root, "docs", "src", "content", "docs", "guides")

if (!requireNamespace("roxygen2", quietly = TRUE)) {
  stop(
    "The roxygen2 package is required to generate documentation before building the reference.",
    call. = FALSE
  )
}

roxygen2::roxygenise(package.dir = project_root)

dir.create(reference_dir, recursive = TRUE, showWarnings = FALSE)

if (!dir.exists(man_dir)) {
  stop("Could not find man directory at: ", man_dir)
}

rd_files <- list.files(man_dir, pattern = "\\.Rd$", full.names = TRUE)
if (!length(rd_files)) {
  stop("No .Rd files found in ", man_dir)
}

topic_slug_lookup <- new.env(parent = emptyenv())
guide_slug_lookup <- new.env(parent = emptyenv())

reference_href <- function(route_slug, from_index = FALSE) {
  prefix <- if (from_index) "./" else "../"
  paste0(prefix, route_slug, "/")
}

guide_href <- function(guide_slug) {
  paste0("../guides/", guide_slug, "/")
}

normalize_docs_href <- function(target) {
  if (!nzchar(target)) {
    return(target)
  }

  if (grepl("^(https?:|mailto:|#)", target)) {
    return(target)
  }

  if (startsWith(target, "/guides/") || startsWith(target, "/reference/")) {
    return(paste0("../..", target))
  }

  target
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
  x <- gsub("\\", "\\\\", x, fixed = TRUE)
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

guide_topics <- if (dir.exists(guides_dir)) {
  tools::file_path_sans_ext(basename(list.files(guides_dir, pattern = "\\.md$", full.names = TRUE)))
} else {
  character()
}

for (guide_slug in guide_topics) {
  assign(guide_slug, guide_slug, envir = guide_slug_lookup)
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
  if (identical(rd_tag, "\\examples")) {
    lines <- lines[!trim(lines) %in% c("## Not run:", "## End(Not run)")]
  }
  compact_blank_lines(lines)
}

resolve_link_slug <- function(target) {
  if (!nzchar(target)) {
    return(NULL)
  }

  if (exists(target, envir = topic_slug_lookup, inherits = FALSE)) {
    return(get(target, envir = topic_slug_lookup, inherits = FALSE))
  }

  slug <- slug_from_symbol(target)
  if (exists(slug, envir = topic_slug_lookup, inherits = FALSE)) {
    return(get(slug, envir = topic_slug_lookup, inherits = FALSE))
  }

  NULL
}

route_slug_from_topic_slug <- function(topic_slug) {
  tolower(topic_slug)
}

render_rd_text <- function(node) {
  if (is.character(node)) {
    return(paste(node, collapse = ""))
  }

  tag <- attr(node, "Rd_tag")

  if (is.null(tag)) {
    return(paste(vapply(as.list(node), render_rd_text, character(1)), collapse = ""))
  }

  if (identical(tag, "TEXT")) {
    return(paste(as.character(node), collapse = ""))
  }

  if (identical(tag, "\\describe")) {
    items <- node[vapply(node, function(child) {
      identical(attr(child, "Rd_tag"), "\\item")
    }, logical(1))]
    return(paste(vapply(items, function(item) {
      label <- trim(render_rd_text(item[[1]]))
      description <- trim(render_rd_text(item[[2]]))
      paste0("- ", label, ": ", description)
    }, character(1)), collapse = "\n"))
  }

  children <- vapply(as.list(node), render_rd_text, character(1))
  content <- paste(children, collapse = "")

  guide_link <- maybe_render_vignette_link(content)
  if (!is.null(guide_link)) {
    return(guide_link)
  }

  if (identical(tag, "\\href")) {
    if (length(node) >= 2) {
      target <- normalize_docs_href(trim(render_rd_text(node[[1]])))
      label <- trim(render_rd_text(node[[2]]))
      if (!nzchar(label)) {
        label <- target
      }
      return(paste0("[", label, "](", target, ")"))
    }

    return(content)
  }

  if (identical(tag, "\\url")) {
    target <- normalize_docs_href(trim(content))
    return(paste0("[", target, "](", target, ")"))
  }

  if (identical(tag, "\\link")) {
    option <- attr(node, "Rd_option")
    target <- if (!is.null(option) && nzchar(option)) {
      sub("^=", "", option)
    } else {
      content
    }

    slug <- resolve_link_slug(target)
    label <- trim(content)
    if (!nzchar(label)) {
      label <- target
    }

    if (!is.null(slug)) {
      return(paste0("[`", label, "`](", reference_href(route_slug_from_topic_slug(slug)), ")"))
    }

    return(paste0("`", label, "`"))
  }

  if (tag %in% c("\\code", "\\pkg", "\\samp", "\\verb", "\\env")) {
    return(paste0("`", trim(content), "`"))
  }

  if (identical(tag, "\\emph")) {
    return(paste0("*", content, "*"))
  }

  if (identical(tag, "\\strong")) {
    return(paste0("**", content, "**"))
  }

  if (tag %in% c("\\eqn", "\\deqn") && length(node) >= 1) {
    math <- trim(render_rd_text(node[[1]]))
    if (identical(tag, "\\deqn")) {
      return(paste0("\n\n$$\n", math, "\n$$\n\n"))
    }

    return(paste0("$", math, "$"))
  }

  content
}

maybe_render_vignette_link <- function(text) {
  match <- regexec('^vignette\\((["\'])([^"\']+)\\1\\)$', trim(text))
  captures <- regmatches(trim(text), match)[[1]]

  if (length(captures) != 3) {
    return(NULL)
  }

  guide_slug <- captures[3]
  if (!exists(guide_slug, envir = guide_slug_lookup, inherits = FALSE)) {
    return(NULL)
  }

  paste0("[`", trim(text), "`](", guide_href(guide_slug), ")")
}

render_rd_block <- function(rd, rd_tag) {
  idx <- which(vapply(rd, function(node) identical(attr(node, "Rd_tag"), rd_tag), logical(1)))[1]
  if (is.na(idx)) {
    return(character())
  }

  text <- render_rd_text(rd[[idx]])
  text <- gsub("\\r", "", text)
  text <- gsub(" +\\n", "\n", text)
  text <- gsub("\\n +", "\n", text)
  text <- gsub("\\n{3,}", "\n\n", text)

  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  lines <- trimws(lines, which = "right")
  compact_blank_lines(lines)
}

format_arguments_rd <- function(rd) {
  idx <- which(vapply(rd, function(node) identical(attr(node, "Rd_tag"), "\\arguments"), logical(1)))[1]
  if (is.na(idx)) {
    return(character())
  }

  args_node <- rd[[idx]]
  items <- args_node[vapply(args_node, function(node) identical(attr(node, "Rd_tag"), "\\item"), logical(1))]
  if (!length(items)) {
    return(character())
  }

  vapply(items, function(item) {
    arg_name <- trim(render_rd_text(item[[1]]))
    arg_desc <- trim(render_rd_text(item[[2]]))
    arg_desc <- gsub("\\s+", " ", arg_desc)
    paste0("- `", arg_name, "`: ", arg_desc)
  }, character(1))
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
    Description = render_rd_block(rd, "\\description"),
    Details = render_rd_block(rd, "\\details"),
    Usage = rd_section_lines(rd, "\\usage", "Usage"),
    Arguments = format_arguments_rd(rd),
    Value = render_rd_block(rd, "\\value"),
    Slots = render_rd_block(rd, "\\section"),
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
    ""
  )

  if (length(sections$Description)) {
    lines <- c(lines, "## Description", "", sections$Description, "")
  }

  if (length(sections$Details)) {
    lines <- c(lines, "## Details", "", sections$Details, "")
  }

  if (length(sections$Usage)) {
    lines <- c(lines, "## Usage", "", "```r", sections$Usage, "```", "")
  }

  if (length(sections$Arguments)) {
    lines <- c(lines, "## Arguments", "", sections$Arguments, "")
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
  aliases <- tryCatch(
    collect_aliases(path),
    error = function(e) {
      stop("Failed to parse aliases from ", basename(path), ": ", conditionMessage(e), call. = FALSE)
    }
  )
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

reference_excluded_functions <- "evalid<-"
function_symbols <- setdiff(
  unique(c(exports$functions, exports$methods)),
  reference_excluded_functions
)

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

for (topic in function_topics) {
  assign(topic$symbol, topic$slug, envir = topic_slug_lookup)
  assign(topic$slug, topic$slug, envir = topic_slug_lookup)
}

for (topic in class_topics) {
  assign(topic$symbol, topic$slug, envir = topic_slug_lookup)
  assign(topic$slug, topic$slug, envir = topic_slug_lookup)
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
  "title: \"Full API Index\"",
  "description: \"Auto-generated reference pages from fiaplyr Rd documentation.\"",
  "---",
  "",
  ""
)

function_group_symbols <- list(
  "Handlers" = c("eval_handler"),
  "Handler Methods" = c(
    "aggregate", "augment", "estimate", "estimate_ratio",
    "evalid", "materialize", "partition",
    "ratio", "show", "subset", "summary", "transform"
  ),
  "Analysis Specifications" = c(
    "grm_analysis", "status_analysis", "dwm_analysis"
  ),
  "Scoped Helpers" = c(
    "fiadb_vt_mini_path",
    "set_fiaplyr_verbosity", "cond", "pcond", "plot", "pplot", "ptree",
    "PostStratifiedEstimator", "PostStratifiedRatioEstimator"
  ),
  "Estimators" = c(
    "pe_post_strat", "pe_post_strat_ratio", "ve_post_strat",
    "ve_post_strat_ratio"
  ),
  "Database Facilitation" = c("database_mapping", "explore_evals"),
  "Growth, Removals and Mortality Macros" = character()
)

section_descriptions <- c(
  "Handlers" = "Create handlers for FIA database analyses.",
  "Handler Methods" = "Subset, transform, estimate, and summarize FIA data.",
  "Analysis Specifications" = "Configure the analysis context used by a handler.",
  "Scoped Helpers" = "Target particular tables within a handler.",
  "Estimators" = "Estimate population totals, ratios, and associated variances.",
  "Database Facilitation" = "Connect analyses to FIA databases and explore their contents.",
  "Growth, Removals and Mortality Macros" = "Build macros for growth, removals, mortality, and related change components.",
  "Down Woody Material Macros" = "Build macros for downed woody material components.",
  "Classes" = "Core S4 classes used to represent handlers, analyses, mappings, and estimators. These are typically not used directly by uses, and their associated lower-case helpers are used instead."
)

macro_symbols <- vapply(
  Filter(
    function(topic) {
      grepl("^(grm_|grom_)", topic$symbol) &&
        !identical(topic$symbol, "grm_analysis")
    },
    function_topics
  ),
  function(topic) topic$symbol,
  character(1)
)
function_group_symbols[["Growth, Removals and Mortality Macros"]] <- macro_symbols

dwm_macro_symbols <- vapply(
  Filter(
    function(topic) {
      grepl("^dwm_", topic$symbol) &&
        !identical(topic$symbol, "dwm_analysis")
    },
    function_topics
  ),
  function(topic) topic$symbol,
  character(1)
)
function_group_symbols[["Down Woody Material Macros"]] <- dwm_macro_symbols

topic_by_symbol <- setNames(function_topics, vapply(function_topics, function(topic) {
  topic$symbol
}, character(1)))
grouped_symbols <- unique(unlist(function_group_symbols, use.names = FALSE))
unclassified_symbols <- setdiff(names(topic_by_symbol), grouped_symbols)
if (length(unclassified_symbols)) {
  function_group_symbols[["Scoped Helpers"]] <- c(
    function_group_symbols[["Scoped Helpers"]],
    unclassified_symbols
  )
}

for (group_name in names(function_group_symbols)) {
  symbols <- function_group_symbols[[group_name]]
  topics <- unname(topic_by_symbol[intersect(symbols, names(topic_by_symbol))])
  if (!length(topics)) {
    next
  }

  index_lines <- c(
    index_lines,
    paste0("## ", group_name),
    "",
    section_descriptions[[group_name]],
    ""
  )
  function_links <- vapply(topics, function(topic) {
    paste0("- [`", topic$symbol, "`](", reference_href(route_slug_from_topic_slug(topic$slug), from_index = TRUE), ")")
  }, character(1))
  index_lines <- c(index_lines, function_links, "")
}

if (length(class_topics)) {
  index_lines <- c(
    index_lines,
    "## Classes",
    "",
    section_descriptions[["Classes"]],
    ""
  )
  class_links <- vapply(class_topics, function(topic) {
    paste0("- [`", topic$symbol, "`](", reference_href(route_slug_from_topic_slug(topic$slug), from_index = TRUE), ")")
  }, character(1))
  index_lines <- c(index_lines, class_links, "")
}

writeLines(index_lines, file.path(reference_dir, "index.md"))
message("Built ", file.path(reference_dir, "index.md"))

if (length(missing_functions) || length(missing_classes)) {
  stop(
    "Some exported topics were not found in man/. Missing functions: ",
    paste(missing_functions, collapse = ", "),
    "; missing classes: ",
    paste(missing_classes, collapse = ", "),
    call. = FALSE
  )
}
