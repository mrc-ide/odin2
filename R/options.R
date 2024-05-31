##' Create a set of odin options.
##'
##' @title Odin options
##'
##' @param verbose Control verbosity of generation, compilation or
##'   validation.  Defaults to the value of the option `odin.verbose`
##'   if not given.
##'
##' @return A list with class `odin_options`
##' @export
odin_options <- function(verbose = NULL) {
  call <- environment()
  verbose <- verbose %||% getOption("odin.verbose")
  assert_scalar_logical(verbose, call = call)
  ret <- list(verbose = verbose)
  class(ret) <- "odin_options"
  ret
}
