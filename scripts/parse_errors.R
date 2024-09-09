#!/usr/bin/env Rscript
read_errors <- function() {
  path_rmd <- "vignettes/errors.Rmd"
  txt <- readLines(path_rmd)

  re <- "^# `(E[0-9]{4})`$"
  i <- grep(re, txt)
  if (length(setdiff(grep("^# ", txt), i)) > 0) {
    stop("Some headings don't match expected pattern")
  }

  f <- function(from, to) {
    ret <- txt[from:to]
    while (ret[[1]] == "") {
      ret <- ret[-1]
    }
    while (ret[[length(ret)]] == "") {
      ret <- ret[-length(ret)]
    }
    ret
  }

  ret <- Map(f, i + 1, c(i[-1] - 1, length(txt)))
  names(ret) <- sub(re, "\\1", txt[i])
  ret
}

parse_node <- function(x) {
  nm <- xml2::xml_name(x)
  switch(nm,
         paragraph = parse_paragraph(x),
         code_block = parse_code_block(x),
         list = parse_list(x),
         ## Hard, inline:
         link = parse_link(x),
         ## Easy, inline:
         code = sprintf("{.code %s}", xml2::xml_text(x)),
         emph = sprintf("{.emph %s}", xml2::xml_text(x)),
         text = xml2::xml_text(x),
         stop(sprintf("Unknown node '%s'", nm)))
}


parse_list <- function(x) {
  items <- xml2::xml_children(x)
  stopifnot(all(vapply(items, xml2::xml_name, "") == "item"))
  items <- lapply(items, function(x) parse_node(xml2::xml_child(x)))
  list(type = "list",
       mode = xml2::xml_attr(x, "type"),
       items = items)
}

parse_paragraph <- function(x) {
  txt <- vapply(xml2::xml_children(x), parse_node, "")
  list(type = "paragraph",
       text = paste(txt, collapse = ""))
}

parse_code_block <- function(x) {
  list(type = "code_block",
       text = strsplit(sub("\n$", "", xml2::xml_text(x)), "\n")[[1]])
}

parse_link <- function(x) {
  target <- xml2::xml_attr(x, "destination")
  if (grepl("^#e[0-9]{4}$", target)) {
    code <- xml2::xml_text(x)
    stopifnot(tolower(code) == sub("^#", "", target))
    sprintf('{.run odin2::odin_error_explain("%s")}', code)
  } else {
    txt <- paste(vapply(xml2::xml_children(x), parse_node, ""), collapse = "")
    sprintf("{.href [%s](%s)}", target, txt)
  }
}

parse_md <- function(md) {
  x <- xml2::read_xml(commonmark::markdown_xml(md))
  lapply(xml2::xml_children(x), parse_node)
}

parse_error <- function(code, txt) {
  list(code = code,
       plain = txt,
       parsed = parse_md(txt))
}

errors_txt <- read_errors()
errors <- Map(parse_error, names(errors_txt), errors_txt)

message("Checking errors render")
pkgload::load_all(quiet = TRUE)
for (err in errors) {
  testthat::evaluate_promise(render_error(err, TRUE))
}

message("Saving sysdata.rda")
save(list = "errors", file = file.path("R/sysdata.rda"), version = 2)
