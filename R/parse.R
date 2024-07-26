odin_parse <- function(expr, input_type = NULL) {
  call <- environment()
  dat <- parse_prepare(rlang::enquo(expr), input_type, call)
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

  parse_check_usage(exprs, ret, call)

  ret
}


parse_check_usage <- function(exprs, dat, call) {
  parse_check_usage_find_unknown(exprs, dat, call)
}


parse_check_usage_find_unknown <- function(exprs, dat, call) {
  known <- c(unlist(dat$storage$contents, FALSE, FALSE), "time", "dt")
  eqs <- c(dat$phases$update$variables,
           dat$phases$deriv$variables,
           dat$phases$output$variables,
           dat$phases$initial$variables,
           dat$phases$compare$compare,
           unname(dat$equations))
  unknown <- lapply(eqs, function(eq) setdiff(eq$rhs$depends$variables, known))
  err <- lengths(unknown) > 0
  if (!any(err)) {
    return()
  }

  err_nms <- unique(unlist(unknown))
  src <- lapply(eqs[err], "[[", "src")
  odin_parse_error(
    "Unknown variable{?s} used in odin code: {squote(err_nms)}",
    "E2006", src, call)
}
