parse_prepare <- function(quo, input, call) {
  info <- parse_prepare_detect(quo, input, call)
  filename <- NULL

  if (info$type == "expression") {
    src <- NULL
    if (rlang::is_call(info$value, "quote")) {
      cli::cli_abort(
        "You have an extra layer of quote() around 'expr'",
        arg = "expr", call = call)
    }
    if (!rlang::is_call(info$value, "{")) {
      cli::cli_abort(
        "Expected 'expr' to be a multiline expression within curly braces",
        arg = "expr", call = call)
    }
    exprs <- lapply(as.list(info$value[-1]), function(x) list(value = x))
  } else {
    if (info$type == "file") {
      filename <- info$value
      exprs <- parse(file = filename, keep.source = TRUE)
    } else {
      exprs <- parse(text = info$value, keep.source = TRUE)
    }
    start <- utils::getSrcLocation(exprs, "line", first = TRUE)
    end <- utils::getSrcLocation(exprs, "line", first = FALSE)
    src_ref <- as.character(attr(exprs, "wholeSrcref", exact = TRUE))
    src_str <- vcapply(seq_along(exprs), function(i) {
      paste(src_ref[start[[i]]:end[[i]]], collapse = "\n")
    })
    exprs <- Map(list,
                 value = as.list(exprs),
                 start = start,
                 end = end,
                 str = src_str)
  }

  list(type = info$type,
       filename = filename,
       exprs = exprs)
}


parse_prepare_detect <- function(quo, input, call) {
  expr <- rlang::quo_get_expr(quo)
  envir <- rlang::quo_get_env(quo)

  ## First, resolve and redirection via symbols:
  if (rlang::is_symbol(quo)) {
    sym <- rlang::as_name(expr)
    if (!rlang::env_has(envir, sym, inherit = TRUE)) {
      cli::cli_abort("Could not find expression '{sym}'",
                     arg = "expr", call = call)
    }
    expr <- rlang::env_get(envir, sym, inherit = TRUE)
  }

  if (!is.null(input)) {
    input <- match_value(input, c("file", "text", "expression"))
  }

  if (is.language(expr)) {
    if (!is.null(input) && input != "expression") {
      cli::cli_abort(
        c("Invalid input for odin; given expression but expected {input}",
          i = "You have requested '{input}' only via the 'input' argument"),
        arg = "expr", call = call)
    }
    input <- "expression"
  } else if (is.character(expr)) {
    if (is.null(input)) {
      if (length(expr) != 1 || grepl("([\n;=]|<-)", expr)) {
        input <- "text"
      } else if (file.exists(expr)) {
        input <- "file"
      } else {
        cli::cli_abort(
          "'{expr}' looks like a filename but does not exist",
          arg = "expr", call = call)
      }
    } else if (input == "expression") {
      cli::cli_abort(
        c("Invalid input for odin; given string but expected {input}",
          i = "You have requested '{input}' only via the 'input' argument"),
        arg = "expr", call = call)
    } else if (input == "file") {
      if (length(expr) != 1) {
        cli::cli_abort(
          c("Invalid input for odin; expected a scalar for 'expr'",
            i = paste("You have requested 'input = \"file\" but provided",
                      "'expr' with length {length(expr)}")),
          arg = "expr", call = call)
      }
      ## TODO: check for length and containing newlines
      if (!file.exists(expr)) {
        cli::cli_abort("File '{expr}' does not exist",
                       arg = "expr", call = call)
      }
    }
  }
  list(type = input, value = expr)
}
