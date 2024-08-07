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


match_value <- function(x, choices, name = deparse(substitute(x)), arg = name,
                        call = NULL) {
  assert_scalar_character(x, call = call, arg = arg)
  if (!(x %in% choices)) {
    choices_str <- paste(squote(choices), collapse = ", ")
    cli::cli_abort(c("'{name}' must be one of {choices_str}",
                     i = "Instead we were given '{x}'"), call = call,
                   arg = arg)
  }
  x
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
