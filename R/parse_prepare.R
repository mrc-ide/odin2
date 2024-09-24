parse_prepare <- function(quo, input_type, call) {
  info <- parse_prepare_detect(quo, input_type, call)
  filename <- NULL

  if (info$type == "expression") {
    src <- NULL
    if (rlang::is_call(info$value, "quote")) {
      cli::cli_abort(
        "You have an extra layer of 'quote()' around 'expr'",
        arg = "expr", call = call)
    }
    if (!rlang::is_call(info$value, "{")) {
      cli::cli_abort(
        "Expected 'expr' to be a multiline expression within curly braces",
        arg = "expr", call = call)
    }
    exprs <- as.list(info$value[-1])
    exprs <- Map(list,
                 value = exprs,
                 index = seq_along(exprs))
  } else {
    if (info$type == "file") {
      filename <- info$value
      exprs <- parse(file = filename, keep.source = TRUE)
    } else {
      str <- paste(info$value, collapse = "\n")
      exprs <- parse(text = str, keep.source = TRUE)
    }
    start <- utils::getSrcLocation(exprs, "line", first = TRUE)
    end <- utils::getSrcLocation(exprs, "line", first = FALSE)
    src_ref <- as.character(attr(exprs, "wholeSrcref", exact = TRUE))
    src_str <- vcapply(seq_along(exprs), function(i) {
      paste(src_ref[start[[i]]:end[[i]]], collapse = "\n")
    })
    exprs <- Map(list,
                 value = as.list(exprs),
                 index = seq_along(exprs),
                 start = start,
                 end = end,
                 str = src_str)
  }

  list(type = info$type,
       filename = filename,
       exprs = exprs)
}


parse_prepare_detect <- function(quo, input_type, call) {
  expr <- rlang::quo_get_expr(quo)
  envir <- rlang::quo_get_env(quo)

  ## First, resolve and redirection via symbols:
  if (rlang::is_symbol(expr)) {
    sym <- rlang::as_name(expr)
    if (!rlang::env_has(envir, sym, inherit = TRUE)) {
      cli::cli_abort("Could not find expression '{sym}'",
                     arg = "expr", call = call)
    }
    expr <- rlang::env_get(envir, sym, inherit = TRUE)
  }

  if (!is.null(input_type)) {
    input_type <- match_value(input_type, c("file", "text", "expression"))
  }

  input_is_expression <- rlang::is_call(expr, c("{", "quote")) ||
    (is.language(expr) && any(c("<-", "=") %in% all.names(expr)))

  if (input_is_expression) {
    if (!is.null(input_type) && input_type != "expression") {
      cli::cli_abort(
        c("Invalid input for odin; given expression but expected {input_type}",
          i = paste("You have requested '{input_type}' only via the",
                    "'input_type' argument")),
        arg = "expr", call = call)
    }
    input_type <- "expression"
  } else {
    if (is.language(expr)) {
      expr <- rlang::eval_tidy(expr, env = envir) # or just eval, who knows?
    }

    ## By this point we expect a string:
    if (!is.character(expr)) {
      if (is.null(input_type)) {
        expected <- "a string, character vector or expression"
      } else if (input_type == "expression") {
        expected <- "an expression"
      } else if (input_type == "file") {
        expected <- "a string"
      } else if (input_type == "text") {
        expected <- "a string or character vector"
      }
      cli::cli_abort("Invalid input for odin; expected {expected}",
                     arg = "expr", call = call)
    }

    if (is.null(input_type)) {
      if (length(expr) != 1 || grepl("([\n;=]|<-)", expr)) {
        input_type <- "text"
      } else if (file.exists(expr)) {
        input_type <- "file"
      } else {
        cli::cli_abort(
          "'{expr}' looks like a filename but does not exist",
          arg = "expr", call = call)
      }
    } else if (input_type == "expression") {
      cli::cli_abort(
        c("Invalid input for odin; given string but expected {input_type}",
          i = paste("You have requested '{input_type}' only via the",
                    "'input_type' argument")),
        arg = "expr", call = call)
    } else if (input_type == "file") {
      if (length(expr) != 1) {
        cli::cli_abort(
          c("Invalid input for odin; expected a scalar for 'expr'",
            i = paste("You have requested 'input_type = \"file\" but provided",
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
  list(type = input_type, value = expr)
}
