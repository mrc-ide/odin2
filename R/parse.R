odin_parse <- function(expr, input_type = NULL) {
  call <- environment()
  dat <- parse_prepare(rlang::enquo(expr), input_type, call)
  NULL
}
