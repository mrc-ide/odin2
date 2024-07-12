odin_parse <- function(expr, input = NULL) {
  call <- environment()
  dat <- parse_prepare(rlang::enquo(expr), input, call)
  NULL
}
