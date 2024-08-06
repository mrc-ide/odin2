odin_parse <- function(expr, input_type = NULL, compatibility = "warning") {
  call <- environment()
  odin_parse_quo(rlang::enquo(expr), input_type, compatibility, call)
}


odin_parse_quo <- function(quo, input_type, compatibility, call) {
  match_value(compatibility, c("silent", "warning", "error"), call = call)
  dat <- parse_prepare(quo, input_type, call)
  dat$exprs <- parse_compat(dat$exprs, compatibility, call)
  exprs <- lapply(dat$exprs, function(x) parse_expr(x$value, x, call = call))
  system <- parse_system_overall(exprs, call)
  equations <- parse_system_depends(
    system$exprs$equations, system$variables, call)
  phases <- parse_system_phases(
    system$exprs, equations, system$variables, system$data$name, call)
  storage <- parse_storage(
    equations, phases, system$variables, system$data, call)
  zero_every <- parse_zero_every(system$time, phases, equations,
                                 system$variables, call)

  ret <- list(time = system$time,
              class = "odin",
              variables = system$variables,
              parameters = system$parameters,
              equations = equations,
              phases = phases,
              storage = storage,
              zero_every = zero_every,
              data = system$data)

  parse_check_usage(exprs, ret, call)
  ret <- parse_adjoint(ret)

  ret
}


parse_check_usage <- function(exprs, dat, call) {
  parse_check_usage_find_unknown(exprs, dat, call)
}


parse_check_usage_find_unknown <- function(exprs, dat, call) {
  implicit <- c("time", if (dat$time == "discrete") "dt")
  known <- c(unlist(dat$storage$contents, FALSE, FALSE), implicit)
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

  if (dat$time == "continuous" && "dt" %in% unknown) {
    uses_dt <- vlapply(err_nms, function(nms) "dt" %in% nms)
    odin_parse_error(
      c("Cannot use 'dt' in a continuous time (ODE) model",
        i = paste("The special variable 'dt' only exists in discrete-time",
                  "models.  In an ODE model, the step size is never known",
                  "by the system, which needs to consider only rates",
                  "of change in the variables.")),
      "E2007", src[uses_dt], call)
  } else {
    odin_parse_error(
      "Unknown variable{?s} used in odin code: {squote(err_nms)}",
      "E2006", src, call)
  }
}
