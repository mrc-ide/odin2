parse_system_overall <- function(exprs, call) {
  special <- vcapply(exprs, function(x) x$special %||% "")
  is_update <- special == "update"
  is_deriv <- special == "deriv"
  is_output <- special == "output"
  is_initial <- special == "initial"
  is_compare <- special == "compare"
  is_data <- special == "data"
  is_parameter <- special == "parameter"
  is_equation <- special %in% c("", "parameter")

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


parse_system_depends <- function(equations, variables, call) {
  automatic <- c("time", "dt")
  implicit <- c(variables, automatic)

  ## First, compute the topological order ignoring variables
  names(equations) <- vcapply(equations, function(eq) eq$lhs$name)
  deps <- lapply(equations, function(eq) {
    ## In an earlier proof-of-concept here we also removed eq$lhs$name
    ## from the dependencies - we do need to do that for arrays at
    ## least, so at some point some more effort is required here.
    setdiff(eq$rhs$depends$variables, implicit)
  })
  res <- topological_order(deps)
  if (!res$success) {
    nms <- names(deps)[res$error]
    details <- vcapply(nms, function(x) {
      sprintf("%s: depends on: %s", x, paste(deps[[x]], collapse = ", "))
    })
    src <- unname(lapply(equations[res$error], "[[", "src"))
    odin_parse_error(
      c("Cyclic dependency detected within equation{?s} {squote(nms)}",
        set_names(details, "i")),
      "E2005", src, call)
  }

  ## Now, we need to get the variables a second time, and only exclude
  ## automatic variables
  deps <- lapply(equations[res$value], function(eq) {
    setdiff(eq$rhs$depends$variables, automatic)
  })
  deps_recursive <- list()
  for (nm in names(deps)) {
    vars <- deps[[nm]]
    deps_recursive[[nm]] <- union(
      vars,
      unlist(deps_recursive[vars], FALSE, FALSE))
    equations[[nm]]$rhs$depends$variables_recursive <- deps_recursive[[nm]]
  }

  equations[names(deps)]
}


parse_system_phases <- function(exprs, equations, variables, data, call) {
  ## First compute the 'stage' that things occur in; there are only
  ## three of these, but "time" covers a multitude of sins and
  ## includes things like the compare function as well as deriv/update
  ## (and in the case of mixed models *both* deriv/update are
  ## considered time).
  stages <- c(system_create = 1,
              parameter_update = 2,
              time = 3,
              data = 4)
  implicit <- c(variables, "time", "dt")
  stage <- c(
    set_names(rep(stages[["time"]], length(implicit)), implicit),
    set_names(rep(stages[["data"]], length(data)), data))
  for (nm in names(equations)) {
    rhs <- equations[[nm]]$rhs
    if (identical(rhs$type, "parameter")) {
      is_constant <- isTRUE(rhs$args$constant)
      stage[[nm]] <-
        stages[[if (is_constant) "system_create" else "parameter_update"]]
    } else {
      stage[[nm]] <- max(stages[["system_create"]],
                         stage[rhs$depends$variables])
    }
  }
  stage <- set_names(names(stages)[stage], names(stage))

  ## Now, we try and work out which parts of the graph are needed at
  ## different "phases".  These roughly correspond to dust functions.

  deps_recursive <- lapply(equations, function(x) {
    x$rhs$depends$variables_recursive
  })

  used <- character()
  required <- character()

  phase_names <- c("update", "deriv", "output", "initial", "compare")
  phases <- set_names(vector("list", length(phase_names)), phase_names)

  for (phase in phase_names) {
    e <- exprs[[phase]]
    if (length(e) > 0) {
      deps <- unique(unlist(lapply(e, function(x) x$rhs$depends$variables),
                            FALSE, FALSE))
      eqs <- intersect(names(equations), deps)
      eqs <- union(eqs, unlist(deps_recursive[eqs], FALSE, FALSE))
      used <- union(used, eqs)

      is_time <- stage[eqs] == "time"
      is_data <- stage[eqs] == "data"
      eqs_time <- intersect(names(equations), eqs[is_time])
      unpack <- intersect(variables, c(eqs, deps))
      required <- union(required, eqs[!(is_time | is_data)])

      if (phase %in% c("update", "deriv", "output")) {
        phases[[phase]] <- list(unpack = unpack,
                                equations = eqs_time,
                                variables = e)
      } else if (phase == "initial") {
        ## I forget what the trick was here, but there's some extra
        ## effort required.
        if (length(unpack) > 0) {
          odin_parse_error(
            "Dependencies within initial conditions not yet supported",
            "E0001", NULL, call)
        }
        phases[[phase]] <- list(equations = eqs_time,
                                variables = e)
      } else if (phase == "compare") {
        eqs_data <- intersect(names(equations), eqs[is_time | is_data])
        phases[[phase]] <- list(equations = eqs_data,
                                unpack = unpack,
                                compare = e)
      }
    }
  }

  eqs_shared <- intersect(names(equations), required)
  phases$build_shared <- list(equations = eqs_shared)
  phases$update_shared <- list(
    equations = eqs_shared[stage[eqs_shared] == "parameter_update"])

  phases
}


parse_storage <- function(equations, phases, variables, data, call) {
  ## This will need additional work once we support arrays as these
  ## will be put into internal storage.
  shared <- intersect(names(equations), phases$build_shared$equations)
  stack <- setdiff(names(equations), shared)

  contents <- list(
    variables = variables,
    shared = shared,
    internal = character(),
    data = data$name,
    output = character(),
    stack = stack)
  location <- set_names(rep(names(contents), lengths(contents)),
                        unlist(contents, FALSE, TRUE))
  location[location == "variables"] <- "state"

  ## We'll need integer variables soon, these are always weird.  We
  ## could also use proper booleans too.
  type <- set_names(rep("real_type", length(location)), names(location))

  ## This will change soon, as we'll need more flexibility with
  ## arrays, and output, and adjoints.  For now just record the
  ## locations of variables and we'll work out what index these sit at
  ## later.
  packing <- list(state = list(scalar = variables))

  list(contents = contents,
       location = location,
       type = type,
       packing = packing)
}


parse_zero_every <- function(time, phases, equations, variables, call) {
  zero_every <- lapply(phases$initial$variables, function(eq) {
    eq$lhs$args$zero_every
  })
  i <- !vlapply(zero_every, is.null)
  if (!any(i)) {
    return(NULL)
  }

  names(zero_every) <- variables
  zero_every <- zero_every[i]

  is_zero <- function(expr) {
    identical(expr, 0) || identical(expr, 0L)
  }

  ## If time is continuous, we should also check that the reset
  ## variables don't reference any other variables, even indirectly;
  ## do this as mrc-5615.

  zero_every
}
