odin_parse <- function(expr, input_type = NULL) {
  call <- environment()
  dat <- parse_prepare(rlang::enquo(expr), input_type, call)
  exprs <- lapply(dat$exprs, function(x) parse_expr(x$value, x, call = call))

  system <- parse_system_overall(exprs, call)
  ret <- list(time = system$time,
              class = "odin",
              variables = system$variables,
              parameters = system$parameters,
              data = system$data)

  ret
}
