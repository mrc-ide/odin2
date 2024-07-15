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

  src <- lapply(exprs, "[[", "src")

  ## Check what sort of system we even have:
  is_continuous <- any(is_deriv)
  is_discrete <- any(is_update)
  if (is_continuous && is_discrete) {
    odin_parse_error(
      "Can't support both 'update()' and 'deriv()' within a single model yet",
      "E0001", src[is_deriv | is_update], call)
  }

  if (any(is_output)) {
    odin_parse_error(
      "Can't support both 'output()' yet",
      "E0001", src[is_output], call)
  }

  ## TODO: names must not be duplicated.  This check is quite hard to
  ## get right because of arrays, and it might be best done elsewhere
  ## as a result?

  target <- if (is_continuous) "deriv" else "update"
  is_target <- special == target

  variables_target <- name_data[is_target]
  if (!setequal(variables, variables_target)) {
    common <- intersect(variables, variables_target)
    err <- (is_target | is_initial) & !(variables %in% common)
    odin_parse_error(
      "Different equations for 'initial()' and '{target}()'",
      "E2002", src[err], call)
  }

  is_differentiable <-
    vlapply(exprs[is_parameter], function(x) x$rhs$args$differentiate)
  ## Later we will want to look this up, after working out what is
  ## going on with arrays.
  is_constant <- vlapply(exprs[is_parameter], function(x) x$rhs$args$constant)

  parameters <- data_frame(
    name = name_data[is_parameter],
    differentiate = is_differentiable,
    constant = is_constant)

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
       data = name_data[is_data],
       exprs = exprs)
}


parse_system_depends <- function(equations, variables) {
  implicit <- c(variables, "time", "dt")
  stages <- c(system_create = 1,
              parameter_update = 2,
              time = 3)
  stage <- c(
    set_names(rep(stages[["time"]], length(implicit)), implicit))

  names(equations) <- vcapply(equations, function(x) x$lhs$name)
  deps <- lapply(equations, function(eq) {
    setdiff(eq$rhs$depends$variables, c(eq$lhs$name, implicit))
  })
  deps <- deps[topological_order(deps)]

  deps_recursive <- list()
  for (nm in names(deps)) {
    vars <- deps[[nm]]
    deps_recursive[[nm]] <- union(
      vars,
      unlist(deps_recursive[vars], FALSE, FALSE))
    rhs <- equations[[nm]]$rhs
    if (identical(rhs$type, "parameter")) {
      is_constant <- isTRUE(rhs$args$constant)
      stage[[nm]] <-
        stages[[if (is_constant) "system_create" else "parameter_update"]]
    } else {
      stage[[nm]] <- max(stages[["system_create"]],
                         stage[rhs$depends$variables])
    }
    equations[[nm]]$rhs$depends$variables_recursive <- deps_recursive[[nm]]
    equations[[nm]]$stage <- names(stages)[[stage[[nm]]]]
  }

  equations[names(deps)]
}


parse_system_phases <- function(exprs, equations, variables) {
  phases <- list()
  used <- character()

  stage <- vcapply(equations, function(x) x$stage)

  deps_recursive <- lapply(equations, function(x) {
    x$rhs$depends$variables_recursive
  })

  skip <- c("equations", "parameter", "data")
  required <- character()

  for (phase in setdiff(names(exprs), skip)) {
    e <- exprs[[phase]]
    if (length(e) > 0) {
      deps <- unique(unlist(lapply(e, function(x) x$rhs$depends$variables),
                            FALSE, FALSE))
      eqs <- intersect(names(equations), deps)
      eqs <- union(eqs, unlist(deps_recursive[eqs], FALSE, FALSE))
      used <- union(used, eqs)

      eqs_time <- intersect(names(equations), eqs[stage[eqs] == "time"])
      unpack <- intersect(deps, variables)
      required <- union(required, eqs[stage[eqs] != "time"])

      if (phase %in% c("update", "deriv")) {
        phases[[phase]] <- list(unpack = unpack,
                                equations = eqs_time,
                                variables = e)
      } else if (phase == "initial") {
        if (length(unpack) > 0) {
          stop("Solve initial dependencies")
        }
        phases[[phase]] <- list(equations = eqs_time,
                                variables = e)
      } else if (phase == "compare") {
        phases[[phase]] <- list(equations = eqs_time,
                                unpack = unpack,
                                compare = e)
      } else {
        stop(sprintf("Unsupported phase '%s'", phase)) # output
      }
    }
  }

  eqs_shared <- intersect(names(equations), required)
  phases$build_shared <- list(equations = eqs_shared)
  phases$update_shared <- list(
    equations = eqs_shared[stage[eqs_shared] == "parameter_update"])

  phases
}


parse_system_location <- function(equations, variables, data) {
  stage <- vcapply(equations, "[[", "stage")

  shared <- names(stage)[stage != "time"]

  contents <- list(
    variables = variables,
    shared = shared,
    internal = character(),
    data = data,
    output = character(),
    stack = names(stage)[stage == "time"])
  location <- set_names(rep(names(contents), lengths(contents)),
                        unlist(contents, FALSE, TRUE))
  location[location == "variables"] <- "state"
  type <- set_names(rep("real_type", length(location)), names(location))
  ## This will change soon, as we'll need more flexibility with
  ## arrays, and output, and adjoints.
  packing <- list(
    state = set_names(as.list(seq_along(variables) - 1), variables))

  list(contents = contents,
       location = location,
       type = type,
       packing = packing)
}
