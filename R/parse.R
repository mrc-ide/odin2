odin_parse <- function(expr, input_type = NULL) {
  call <- environment()
  odin_parse_quo(rlang::enquo(expr), input_type, call)
}


odin_parse_quo <- function(quo, input_type, call) {
  dat <- parse_prepare(quo, input_type, call)
  exprs <- lapply(dat$exprs, function(x) parse_expr(x$value, x, call = call))

  system <- parse_system_overall(exprs, call)
  equations <- parse_system_depends(
    system$exprs$equations, system$variables, call)
  phases <- parse_system_phases(
    system$exprs, equations, system$variables, system$data$name, call)
  storage <- parse_storage(
    equations, phases, system$variables, system$data, call)

  ret <- list(time = system$time,
              class = "odin",
              variables = system$variables,
              parameters = system$parameters,
              equations = equations,
              phases = phases,
              storage = storage,
              data = system$data)

  ret
}
