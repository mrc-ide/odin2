##' Compile an odin model, yielding a `dust_system_generator` object.
##'
##' @title Compile an odin model
##'
##' @param expr Odin code as the path to a file (a string), a
##'   character vector of code, or as an expression (typically within
##'   braces `{}`).
##'
##' @param input_type An optional string describing the type of input
##'   for `expr` - must be one of `file`, `text` or `expression`.  If
##'   given, this skips the type detection logic and odin will throw
##'   an error if the wrong type of input is given.  Using this may be
##'   beneficial in programmatic environments.
##'
##' @param compatibility Compatibility mode to use.  Valid options are
##'   "warning", which updates code that can be fixed, with warnings,
##'   and "error", which will error.  The option "silent" will
##'   silently rewrite code, but this is not recommended for general
##'   use as eventually the compatibility mode will be removed (this
##'   option is primarily intended for comparing output of odin1 and
##'   odin2 models against old code).
##'
##' @inheritParams dust2::dust_compile
##'
##' @return A `dust_system_generator` object, suitable for using with
##'   dust functions (starting from [dust2::dust_system_create])
##'
##' @export
odin <- function(expr, input_type = NULL, quiet = NULL, workdir = NULL,
                 debug = FALSE, skip_cache = FALSE, compatibility = "warning") {
  call <- environment()
  dat <- odin_parse_quo(rlang::enquo(expr), input_type, compatibility, call)
  code <- generate_dust_system(dat)
  tmp <- tempfile(fileext = ".cpp")
  on.exit(unlink(tmp))
  writeLines(code, tmp)
  dust2::dust_compile(tmp, quiet = quiet, workdir = workdir, debug = debug,
                      skip_cache = skip_cache)
}



##' Show generated code from compiling an odin model.
##'
##' @title Show generated odin code
##'
##' @inheritParams odin
##'
##' @param what Optional string, being a single method to show.
##'   Popular options are `update`, `rhs` and `compare_data`.
##'
##' @return A character vector, with class `odin_code` that has a
##'   pretty-print method defined.  Returns `NULL` if `what` was given
##'   but the model lacks this part.
##'
##' @export
odin_show <- function(expr, input_type = NULL, compatibility = "warning",
                      what = NULL) {
  call <- environment()
  dat <- odin_parse_quo(rlang::enquo(expr), input_type, compatibility, call)
  if (!is.null(what)) {
    parts <- generate_dust_parts()
    dat <- generate_prepare(dat)
    code <- parts[[match_value(what, names(parts))]](dat)
    attr(code, "what") <- what
  } else {
    code <- generate_dust_system(dat)
  }
  if (!is.null(code)) {
    class(code) <- "odin_code"
  }
  code
}


##' @export
print.odin_code <- function(x, ...) {
  ## We can probably do more with this later, but this should already
  ## be enough:
  what <- attr(x, "what")
  if (is.null(what)) {
    cli::cli_h1("odin code:")
  } else {
    cli::cli_h1("odin code ({what}):")
  }
  writeLines(x)
  invisible(x)
}
