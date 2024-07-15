odin_parse <- function(expr, input_type = NULL) {
  call <- environment()
  dat <- parse_prepare(rlang::enquo(expr), input_type, call)
  exprs <- lapply(dat$exprs, function(x) parse_expr(x$value, x, call = call))

  system <- parse_system_overall(exprs, call)
  ## TODO: What has not been done here:
  ## * check for unknown variables
  ## * update recursive dependencies within non-equations bits
  ## * no use of data from update etc
  equations <- parse_system_depends(system$exprs$equations, system$variables)
  phases <- parse_system_phases(system$exprs, equations, system$variables)
  location <- parse_system_location(equations, system$variables, system$data)

  ret <- list(time = system$time,
              name = "odin",
              parameters = system$parameters,
              location = location,
              phases = phases,
              equations = equations)

  ret
}
