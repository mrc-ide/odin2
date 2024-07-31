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
##' @inheritParams dust2::dust_compile
##'
##' @return A `dust_system_generator` object, suitable for using with
##'   dust functions (starting from [dust2::dust_system_create])
##'
##' @export
odin <- function(expr, input_type = NULL, quiet = FALSE, workdir = NULL,
                 debug = FALSE, skip_cache = FALSE) {
  dat <- odin_parse_quo(rlang::enquo(expr), input_type, call)
  code <- generate_dust_system(dat)
  tmp <- tempfile(fileext = ".cpp")
  on.exit(unlink(tmp))
  writeLines(code, tmp)
  dust2::dust_compile(tmp, quiet = quiet, workdir = workdir, debug = debug,
                      skip_cache = skip_cache)
}
