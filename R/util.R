`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


squote <- function(x) {
  sprintf("'%s'", x)
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
