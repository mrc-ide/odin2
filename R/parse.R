odin_parse <- function(expr, input = NULL) {
  call <- environment()
  dat <- parse_prepare(rlang::enquo(expr), input, call)
  exprs <- lapply(dat$exprs, function(x) parse_expr(x$value, x, call = call))
  system <- parse_system(exprs, call)

  ## TODO: at some point we need to filter out some non-equation
  ## things from exprs; just assuming that away here.  This will be
  ## config (once we have any) and print (again, once supported).
  ## It's not clear that user will be wanted here either, but they're
  ## fairly harmless. The dimensions need to be totally gone by this
  ## point too.  For special, we will be left with initial, deriv,
  ## update, output and compare which feels correct to me and will
  ## correspond roughly to "phases"
  implicit <- c(system$variables, TIME, DT)

  ## TODO: What has not been done here:
  ## * check for unknown variables
  ## * update recursive dependencies within non-equations bits
  equations <- parse_depend_equations(system$exprs$equations, implicit)
  phases <- parse_phases(system$exprs, equations, system$variables)
  location <- parse_location(equations, system$variables)

  ret <- list(time = system$time,
              class = "odin",
              location = location,
              phases = phases,
              equations = equations)
  ret
}


parse_system <- function(exprs, call) {
  special <- vcapply(exprs, function(x) x$lhs$special %||% "")
  is_lhs_update <- special == "update"
  is_lhs_deriv <- special == "deriv"
  is_lhs_output <- special == "output"
  is_lhs_initial <- special == "initial"
  is_lhs_compare <- special == "compare"
  is_equation <- special == ""
  name_data <- vcapply(exprs, function(x) x$lhs$name)

  ## We take initial as the set of variables:
  variables <- name_data[is_lhs_initial]
  if (length(variables) == 0) {
    odin_parse_error("Did not find any call to 'initial()'", NULL, call)
  }

  src <- lapply(exprs, "[[", "src")

  ## Check what sort of system we even have:
  is_continuous <- any(is_lhs_deriv)
  is_discrete <- any(is_lhs_update)
  if (is_continuous && is_discrete) {
    odin_parse_error(
      "Can't support both 'update()' and 'deriv()' within a single model yet",
      src[is_lhs_deriv | is_lhs_update], call)
  }

  if (any(is_lhs_output)) {
    odin_parse_error(
      "Can't support both 'output()' yet",
      src[is_lhs_output], call)
  }

  target <- if (is_continuous) "deriv" else "update"
  is_lhs_target <- special == target

  variables_target <- name_data[is_lhs_target]
  if (!setequal(variables, variables_target)) {
    common <- intersect(variables, variables_target)
    err <- (is_lhs_target | is_lhs_initial) & !(variables %in% common)
    odin_parse_error(
      "Different equations for 'initial()' and '{target}()'",
      src[err], call)
  }

  ## Then we break equations up:
  exprs <- list(equations = exprs[is_equation],
                update = exprs[is_lhs_update],
                deriv = exprs[is_lhs_deriv],
                output = exprs[is_lhs_output],
                initial = exprs[is_lhs_initial],
                compare = exprs[is_lhs_compare])
  list(time = if (is_continuous) "continuous" else "discrete",
       variables = variables,
       exprs = exprs)
}


parse_depends <- function(system, call) {
}


parse_depend_equations <- function(equations, implicit) {
  names(equations) <- vcapply(equations, function(x) x$lhs$name)

  deps <- lapply(equations, function(eq) {
    setdiff(eq$rhs$depends$variables, c(eq$lhs$name, implicit))
  })

  deps <- deps[topological_order(deps)]
  stages <- c(constant = 1, time = 2)
  stage <- set_names(rep(stages[["time"]], length(implicit)), implicit)

  deps_recursive <- list()
  for (nm in names(deps)) {
    vars <- deps[[nm]]
    deps_recursive[[nm]] <- union(
      vars,
      unlist(deps_recursive[vars], FALSE, FALSE))
    stage[[nm]] <- max(stages[["constant"]],
                       stage[equations[[nm]]$rhs$depends$variables])
    equations[[nm]]$rhs$depends$variables_recursive <- deps_recursive[[nm]]
    equations[[nm]]$stage <- names(stages)[[stage[[nm]]]]
  }

  equations[names(deps)]
}


## This is going to be the grossest bit I think.
parse_phases <- function(exprs, equations, variables) {
  phases <- list(build_shared = list(equations = character()))
  used <- character()

  stage <- vcapply(equations, function(x) x$stage)
  deps_recursive <- lapply(equations, function(x) {
    x$rhs$depends$variables_recursive
  })

  for (phase in setdiff(names(exprs), "equations")) {
    e <- exprs[[phase]]
    if (length(e) > 0) {
      deps <- unique(unlist(lapply(e, function(x) x$rhs$depends$variables),
                            FALSE, FALSE))
      eqs <- intersect(names(equations), deps)
      eqs <- union(eqs, unlist(deps_recursive[eqs], FALSE, FALSE))
      used <- union(used, eqs)

      eqs_time <- eqs[stage[eqs] == "time"]
      unpack <- intersect(deps, variables)

      ## These bits will change with user variables, and where we
      ## support parameters and updates.  This is fine for now though.
      phases$build_shared$equations <-
        union(phases$build_shared$equations,
              eqs[stage[eqs] == "constant"])

      if (phase == "update") { # also deriv
        phases[[phase]] <- list(unpack = unpack,
                                equations = eqs_time,
                                variables = e)
      } else if (phase == "initial") {
        if (length(unpack) > 0) {
          stop("Solve initial dependencies")
        }
        phases[[phase]] <- list(equations = eqs_time,
                                variables = e)
      } else {
        stop("Unreachable")
      }
    }
  }

  ## Reorder following the dependency graph:
  phases$build_shared$equations <- intersect(
    names(equations),
    phases$build_shared$equations)

  phases
}


parse_location <- function(equations, variables) {
  stage <- vcapply(equations, "[[", "stage")

  contents <- list(
    variables = variables,
    shared = names(stage)[stage == "constant"],
    internal = character(),
    data = character(),
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


odin_parse_error <- function(msg, src, expr, .envir = parent.frame()) {
  cli::cli_abort(msg,
                 class = "odin_parse_error",
                 src = src,
                 expr = expr,
                 call = call,
                 .envir = .envir)
}
