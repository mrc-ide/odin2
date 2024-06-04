odin_parse <- function(expr, input = NULL) {
  call <- environment()
  dat <- parse_prepare(rlang::enquo(expr), input, call)
  exprs <- lapply(dat$exprs, function(x) parse_expr(x$value, x, call = call))
  ## I think this is too much here; we shold break this in two, as we
  ## hold all sorts of exciting things here.
  system <- parse_system(exprs, call)

  ## TODO: What has not been done here:
  ## * check for unknown variables
  ## * update recursive dependencies within non-equations bits
  ## * no use of data from update etc
  equations <- parse_depend_equations(system$exprs$equations, system$variables)
  phases <- parse_phases(system$exprs, equations,
                         system$variables, system$parameters)
  location <- parse_location(equations, system$variables, system$data)

  ret <- list(time = system$time,
              class = "odin",
              location = location,
              phases = phases,
              equations = equations)
  ret
}


parse_system <- function(exprs, call) {
  special <- vcapply(exprs, function(x) x$special %||% "")
  is_update <- special == "update"
  is_deriv <- special == "deriv"
  is_output <- special == "output"
  is_initial <- special == "initial"
  is_compare <- special == "compare"
  is_data <- special == "data"

  is_parameter <- vlapply(exprs, function(x) identical(x$rhs$type, "parameter"))

  is_equation <- special == ""
  ## compare does not have a lhs name
  name_data <- vcapply(exprs, function(x) x$lhs$name %||% "")

  ## We take initial as the set of variables:
  variables <- name_data[is_initial]
  if (length(variables) == 0) {
    odin_parse_error("Did not find any call to 'initial()'", NULL, call)
  }

  src <- lapply(exprs, "[[", "src")

  ## Check what sort of system we even have:
  is_continuous <- any(is_deriv)
  is_discrete <- any(is_update)
  if (is_continuous && is_discrete) {
    odin_parse_error(
      "Can't support both 'update()' and 'deriv()' within a single model yet",
      src[is_deriv | is_update], call)
  }

  if (any(is_output)) {
    odin_parse_error(
      "Can't support both 'output()' yet",
      src[is_output], call)
  }

  ## TODO: names must not be duplicated.  This check is quite hard to
  ## get right because of arrays.

  target <- if (is_continuous) "deriv" else "update"
  is_target <- special == target

  variables_target <- name_data[is_target]
  if (!setequal(variables, variables_target)) {
    common <- intersect(variables, variables_target)
    err <- (is_target | is_initial) & !(variables %in% common)
    odin_parse_error(
      "Different equations for 'initial()' and '{target}()'",
      src[err], call)
  }

  ## Then we break equations up:
  exprs <- list(equations = exprs[is_equation],
                update = exprs[is_update],
                deriv = exprs[is_deriv],
                output = exprs[is_output],
                initial = exprs[is_initial],
                compare = exprs[is_compare],
                data = exprs[is_data])
  list(time = if (is_continuous) "continuous" else "discrete",
       variables = variables,
       parameters = name_data[is_parameter],
       data = name_data[is_data],
       exprs = exprs)
}


parse_depend_equations <- function(equations, variables) {
  implicit <- c(variables, TIME, DT)
  stages <- c(create = 1,
              update = 2,
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
      stage[[nm]] <- stages[[if (is_constant) "create" else "update"]]
    } else {
      stage[[nm]] <- max(stages[["create"]],
                         stage[rhs$depends$variables])
    }
    equations[[nm]]$rhs$depends$variables_recursive <- deps_recursive[[nm]]
    equations[[nm]]$stage <- names(stages)[[stage[[nm]]]]
  }

  equations[names(deps)]
}


## This is going to be the grossest bit I think.
parse_phases <- function(exprs, equations, variables, parameters) {
  phases <- list()
  used <- character()

  ## Why do I have the wrong stage here now?
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

      eqs_time <- eqs[stage[eqs] == "time"]
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
    equations = eqs_shared[stage[eqs_shared] == "update"])

  phases
}


parse_location <- function(equations, variables, data) {
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


odin_parse_error <- function(msg, src, call, .envir = parent.frame()) {
  cli::cli_abort(msg,
                 class = "odin_parse_error",
                 src = src,
                 call = call,
                 .envir = .envir)
}
