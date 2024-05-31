parse_depends <- function(system, call) {
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
  phases <- parse_depend_phases(system$exprs, equations, system$variables)
  location <- parse_depend_location(equations, system$variables)

  list(time = system$time,
       class = "odin",
       location = location,
       phases = phases,
       equations = equations)
}


topological_order <- function(deps) {
  if (all(lengths(deps) == 0)) {
    return(seq_along(deps))
  }

  m <- matrix(FALSE, length(deps), length(deps))
  for (i in seq_along(deps)) {
    m[, i] <- unname(names(deps) %in% deps[[i]])
  }

  pending <- rep(TRUE, length(deps))
  ret <- integer(0)
  while (any(pending)) {
    i <- which(pending)[colSums(m[, pending, drop = FALSE]) == 0]
    if (length(i) > 0L) {
      ret <- c(ret, i)
      pending[i] <- FALSE
      m[i, ] <- FALSE
    } else {
      f <- function(i) {
        sprintf("\t%s: depends on %s", names(ret)[[i]],
                paste(err[m[pending, i]], collapse = ", "))
      }
      err <- names(ret)[pending]
      detail <- paste(vcapply(which(pending), f), collapse = "\n")
      stop(sprintf("A cyclic dependency detected for %s:\n%s",
                   paste(names(ret)[pending], collapse = ", "),
                   detail), call. = FALSE)
    }
  }

  ret
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
parse_depend_phases <- function(exprs, equations, variables) {
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


parse_depend_location <- function(equations, variables) {
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
