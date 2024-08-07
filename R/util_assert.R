assert_scalar <- function(x, name = deparse(substitute(x)), arg = name,
                          call = NULL)  {
  if (length(x) != 1) {
    cli::cli_abort(
      c("'{name}' must be a scalar",
        i = "{name} has length {length(x)}"),
      call = call, arg = arg)
  }
  invisible(x)
}


assert_character <- function(x, name = deparse(substitute(x)),
                             arg = name, call = NULL) {
  if (!is.character(x)) {
    cli::cli_abort("Expected '{name}' to be character", call = call, arg = arg)
  }
  invisible(x)
}


assert_logical <- function(x, name = deparse(substitute(x)),
                          arg = name, call = NULL) {
  if (!is.logical(x)) {
    cli::cli_abort("Expected '{name}' to be logical", arg = arg, call = call)
  }
  invisible(x)
}

assert_nonmissing <- function(x, name = deparse(substitute(x)),
                          arg = name, call = NULL) {
  if (anyNA(x)) {
    cli::cli_abort("Expected '{name}' to be non-NA", arg = arg, call = call)
  }
  invisible(x)
}


assert_scalar_character <- function(x, name = deparse(substitute(x)),
                                  arg = name, call = NULL) {
  assert_scalar(x, name, arg = arg, call = call)
  assert_character(x, name, arg = arg, call = call)
  assert_nonmissing(x, name, arg = arg, call = call)
}
