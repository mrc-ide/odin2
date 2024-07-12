odin_parse <- function(expr, input_type = NULL) {
  call <- environment()
  dat <- parse_prepare(rlang::enquo(expr), input_type, call)
  exprs <- lapply(dat$exprs, function(x) parse_expr(x$value, x, call = call))
  NULL
}


odin_parse_error <- function(msg, src, call, parent = NULL,
                             .envir = parent.frame()) {
  cli::cli_abort(msg,
                 class = "odin_parse_error",
                 src = src,
                 call = call,
                 parent = parent,
                 .envir = .envir)
}
