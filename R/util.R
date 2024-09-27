`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


squote <- function(x) {
  sprintf("'%s'", x)
}


vlapply <- function(...) {
  vapply(..., FUN.VALUE = TRUE)
}


viapply <- function(...) {
  vapply(..., FUN.VALUE = 1L)
}


vcapply <- function(...) {
  vapply(..., FUN.VALUE = "")
}


## See mrc-5614 for ideas about improving this, for later.
match_call <- function(call, fn) {
  ## We'll probably expand on the error case here to return something
  ## much nicer?

  ## TODO: it would be great to totally prevent partial matching here.
  ## The warning emitted by R is not easily caught (no special class
  ## for example) and neither match.call nor the rlang wrapper provide
  ## a hook here to really pick this up.  We can look for expanded
  ## names in the results, though that's not super obvious either
  ## since we're also filling them in and reordering.
  tryCatch(
    list(success = TRUE,
         value = rlang::call_match(call, fn, defaults = TRUE)),
    error = function(e) {
      list(success = FALSE,
           error = e)
    })
}


is_scalar_logical <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
}


is_scalar_character <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}


is_scalar_character <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x)
}


is_scalar_size <- function(x) {
  is.numeric(x) && length(x) == 1 && !is.na(x) && rlang::is_integerish(x) &&
    x > 0
}


collector <- function(init = character(0)) {
  env <- new.env(parent = emptyenv())
  env$res <- init
  list(
    add = function(x) env$res <- c(env$res, x),
    get = function() env$res)
}


data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)
}


set_names <- function(x, nms) {
  if (length(nms) == 1 && length(x) != 1) {
    if (is.null(x)) {
      return(NULL)
    }
    nms <- rep_len(nms, length(x))
  }
  names(x) <- nms
  x
}


cli_nbsp <- function(x) {
  gsub(" ", "\u00a0", x)
}


unlist0 <- function(x) {
  unlist(x, FALSE, FALSE)
}


## This is definitely possible with rlang, but I am not sure how.
substitute_ <- function(expr, env) {
  eval(substitute(substitute(y, env), list(y = expr)))
}


uses_unary_minus <- function(expr) {
  if (rlang::is_call(expr, "-") && length(expr) == 2) {
    return(TRUE)
  } else if (is.recursive(expr)) {
    any(vlapply(expr[-1], uses_unary_minus))
  } else {
    FALSE
  }
}


expr_minus <- function(a, b) {
  monty::monty_differentiation()$maths$minus(a, b)
}


expr_plus <- function(a, b) {
  monty::monty_differentiation()$maths$plus(a, b)
}


expr_times <- function(a, b) {
  monty::monty_differentiation()$maths$times(a, b)
}


expr_sum <- function(x) {
  plus <- monty::monty_differentiation()$maths$plus
  ret <- 0
  for (el in x) {
    ret <- plus(ret, el)
  }
  ret
}


expr_prod <- function(x) {
  times <- monty::monty_differentiation()$maths$times
  ret <- 1
  for (el in x) {
    ret <- times(ret, el)
  }
  ret
}


## From dust2
writelines_if_changed <- function(text, workdir, path, quiet) {
  path_full <- file.path(workdir, path)
  skip <- file.exists(path_full) && same_content(path_full, text)
  if (skip) {
    if (!quiet) {
      cli::cli_alert_info("'{path}' is up to date")
    }
  } else {
    writeLines(text, path_full)
    if (!quiet) {
      cli::cli_alert_success("Wrote '{path}'")
    }
  }
}


same_content <- function(path, text) {
  identical(read_lines(path), paste(as.character(text), collapse = "\n"))
}


read_lines <- function(path) {
  paste(readLines(path), collapse = "\n")
}


dir_create <- function(path) {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
}


last <- function(x) {
  x[[length(x)]]
}


rank_description <- function(rank) {
  switch(rank + 1L,
         "scalar",
         "vector",
         "matrix",
         sprintf("%d-dimensional array", rank))
}


glue_find_variables <- function(string) {
  seen <- collector()
  transformer <-  function(text, envir) {
    seen$add(trimws(text))
    text
  }
  glue::glue(string, .transformer = transformer)
  seen$get()
}
