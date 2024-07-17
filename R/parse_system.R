parse_system_overall <- function(exprs, call) {
  special <- vcapply(exprs, function(x) x$special %||% "")
  is_update <- special == "update"
  is_deriv <- special == "deriv"
  is_output <- special == "output"
  is_initial <- special == "initial"
  is_compare <- special == "compare"
  is_data <- special == "data"
  is_parameter <- special == "parameter"
  is_equation <- special == ""

  ## We take initial as the set of variables:
  variables <- vcapply(exprs[is_initial], function(x) x$lhs$name)
  if (length(variables) == 0) {
    odin_parse_error("Did not find any call to 'initial()'",
                     "E2001", NULL, call)
  }

  ## Check what sort of system we even have:
  is_continuous <- any(is_deriv)
  is_discrete <- any(is_update)
  if (is_continuous && is_discrete) {
    src <- lapply(exprs[is_deriv | is_update], "[[", "src")
    odin_parse_error(
      "Can't use both 'update()' and 'deriv()' within a single model yet",
      "E0001", src, call)
  }

  ## TODO: names must not be duplicated across all equations.  This
  ## check is quite hard to get right because of arrays, and is best
  ## done elsewhere.  For now we'll just ignore this problem...

  target <- if (is_continuous) "deriv" else "update"
  is_target <- special == target

  if (!any(is_target)) {
    src <- lapply(exprs[is_initial], "[[", "src")
    odin_parse_error("Did not find any call to 'deriv()' or 'initial()'",
                     "E2002", NULL, call)
  }

  variables_target <- vcapply(exprs[is_target], function(x) x$lhs$name)

  if (!setequal(variables, variables_target)) {
    msg_initial <- setdiff(variables_target, variables)
    if (length(msg_initial) > 0) {
      err <- which(is_target)[variables_target %in% msg_initial]
      src <- lapply(exprs[err], "[[", "src")
      odin_parse_error(
        c("Variables used in '{target}()' do not have 'initial()' calls",
          x = "Did not find 'initial()' calls for {squote(msg_initial)}"),
        "E2003", src, call)
    }

    msg_target <- setdiff(variables, variables_target)
    if (length(msg_target) > 0) {
      err <- which(is_initial)[variables %in% msg_target]
      src <- lapply(exprs[err], "[[", "src")
      odin_parse_error(
        c("Variables defined with 'initial()' do not have '{target}()' calls",
          x = "Did not find '{target}()' calls for {squote(msg_target)}"),
        "E2004", src, call)
    }
  }

  is_differentiable <-
    vlapply(exprs[is_parameter], function(x) x$rhs$args$differentiate)
  is_constant <- vlapply(exprs[is_parameter], function(x) x$rhs$args$constant)
  if (any(is.na(is_constant))) {
    default_constant <- any(is_differentiable)
    for (i in which(is_parameter)[is.na(is_constant)]) {
      exprs[[i]]$x$rhs$args$constant <- default_constant
    }
    is_constant[is.na(is_constant)] <- default_constant
  }

  parameters <- data_frame(
    name = vcapply(exprs[is_parameter], function(x) x$lhs$name),
    differentiate = is_differentiable,
    constant = is_constant)

  data <- data_frame(
    name = vcapply(exprs[is_data], function(x) x$lhs$name))

  exprs <- list(equations = exprs[is_equation],
                update = exprs[is_update],
                deriv = exprs[is_deriv],
                output = exprs[is_output],
                initial = exprs[is_initial],
                compare = exprs[is_compare],
                data = exprs[is_data])

  list(time = if (is_continuous) "continuous" else "discrete",
       variables = variables,
       parameters = parameters,
       data = data,
       exprs = exprs)
}
