parse_system_overall <- function(exprs, call) {
  special <- vcapply(exprs, function(x) x$special %||% "")
  is_update <- special == "update"
  is_deriv <- special == "deriv"
  is_output <- special == "output"
  is_initial <- special == "initial"
  is_compare <- vlapply(exprs, function(x) rlang::is_call(x$src$value, "~"))
  is_data <- special == "data"
  is_dim <- special == "dim"
  is_parameter <- special == "parameter"
  is_equation <- special %in% c("", "parameter", "dim") & !is_compare

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
  type <- vcapply(exprs[is_parameter], function(x) x$rhs$args$type)
  if (any(is.na(is_constant))) {
    default_constant <- any(is_differentiable)
    for (i in which(is_parameter)[is.na(is_constant)]) {
      exprs[[i]]$x$rhs$args$constant <- default_constant
    }
    is_constant[is.na(is_constant)] <- default_constant
  }

  parameters <- data_frame(
    name = vcapply(exprs[is_parameter], function(x) x$lhs$name),
    type = type,
    differentiate = is_differentiable,
    constant = is_constant)

  data <- data_frame(
    name = vcapply(exprs[is_data], function(x) x$lhs$name))

  dims <- lapply(exprs[is_dim], function(x) x$rhs$value)
  arrays <- data_frame(
    name = vcapply(exprs[is_dim], function(x) x$lhs$name_data),
    rank = lengths(dims),
    dims = I(dims),
    size = I(lapply(dims, expr_prod)))

  is_interpolate <- vlapply(exprs,
                            function(x) identical(x$rhs$type, "interpolate"))
  if (any(is_interpolate)) {
    interpolate_use <- lapply(
      exprs[is_interpolate],
      function(eq) {
        list(
          lhs = list(name = eq$lhs$name_data),
          rhs = list(type = "expression",
                     expr = call("OdinInterpolateEval", eq$lhs$name),
                     depends = list(functions = character(),
                                    variables = eq$lhs$name)),
          src = eq$src)
      })
  } else {
    interpolate_use <- NULL
  }

  exprs <- list(equations = c(exprs[is_equation], interpolate_use),
                update = exprs[is_update],
                deriv = exprs[is_deriv],
                output = exprs[is_output],
                initial = exprs[is_initial],
                compare = exprs[is_compare],
                data = exprs[is_data])

  list(time = if (is_continuous) "continuous" else "discrete",
       variables = variables,
       parameters = parameters,
       arrays = arrays,
       data = data,
       exprs = exprs)
}


parse_system_depends <- function(equations, variables, call) {
  automatic <- c("time", "dt")
  implicit <- c(variables, automatic)

  nms <- vcapply(equations, function(eq) eq$lhs$name)
  ## First, compute the topological order ignoring variables
  names(equations) <- nms
  deps <- collapse_dependencies(lapply(equations, function(eq) {
    ## In an earlier proof-of-concept here we also removed eq$lhs$name
    ## from the dependencies - we do need to do that for arrays at
    ## least, so at some point some more effort is required here.
    ##
    ## TODO: we expect to see a cyclic dependency here now, and we'll
    ## add a test in this PR and then fix this for arrays, but leave
    ## an error in for other equations.
    setdiff(eq$rhs$depends$variables, implicit)
  }))
  res <- topological_order(deps)
  if (!res$success) {
    nms <- names(deps)[res$error]
    details <- vcapply(nms, function(x) {
      sprintf("%s: depends on: %s", x, paste(deps[[x]], collapse = ", "))
    })
    i <- match(nms, names(deps))
    src <- unname(lapply(equations[res$error[i]], "[[", "src"))
    odin_parse_error(
      c("Cyclic dependency detected within equation{?s} {squote(nms)}",
        set_names(details, "i")),
      "E2005", src, call)
  }

  ## Reorder equations by topological order, preserving ordering
  ## within duplicated names:
  equations <- equations[order(match(nms, names(deps)[res$value]))]

  ## Now, we need to get the variables a second time, and only exclude
  ## automatic variables
  deps <- lapply(equations, function(eq) {
    setdiff(eq$rhs$depends$variables, automatic)
  })
  deps_recursive <- list()
  for (nm in names(deps)) {
    vars <- deps[[nm]]
    deps_recursive[[nm]] <- union(
      vars,
      unlist(deps_recursive[vars], FALSE, FALSE))
    for (i in which(names(equations) == nm)) {
      equations[[i]]$rhs$depends$variables_recursive <- deps_recursive[[nm]]
    }
  }

  equations
}


## Next step, make phase (again) a property of a name, not an id.  I
## don't think the alternative is interesting enough to warrant the
## effort.  Then things simplify back out quite nicely again....
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

  stage <- rep(NA_character_, length(equations))

  for (i in seq_along(equations)) {
    rhs <- equations[[i]]$rhs
    vars <- rhs$depends[["variables"]]
    if (identical(rhs$type, "parameter")) {
      is_constant <- isTRUE(rhs$args$constant)
      stage[[i]] <- if (is_constant) "system_create" else "parameter_update"
    } else if (any(vars %in% data)) {
      stage[[i]] <- "data"
    } else if (isTRUE(rhs$is_stochastic) || any(vars %in% implicit)) {
      stage[[i]] <- "time"
    } else if (rlang::is_call(rhs$expr, "OdinInterpolateEval")) {
      stage[[i]] <- "time"
    } else {
      stage_i <- stage[names(equations) %in% vars]
      if (length(stage_i) == 0) {
        stage[[i]] <- "system_create"
      } else {
        stage[[i]] <- names(stages)[[max(stages[stage_i])]]
      }
    }
  }

  if (anyDuplicated(names(equations))) {
    stages_i <- tapply(stages[stage], names(equations), max)
    stage <- set_names(names(stages)[stages_i], names(stages_i))
  } else {
    stage <- set_names(stage, names(equations))
  }

  is_dim <- vlapply(equations, function(x) identical(x$special, "dim"))
  stage_dim <- stage[names(which(is_dim))]
  if (any(stage_dim != "system_create")) {
    ## TODO: this does need a decent error, referencing the invalid
    ## equations and explaining why we think the dimension is time.
    stop("invalid dimension time")
  }

  ## Now, we try and work out which parts of the graph are needed at
  ## different "phases".  These roughly correspond to dust functions.

  deps_recursive <- collapse_dependencies(lapply(equations, function(x) {
    x$rhs$depends$variables_recursive
  }))

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
      eqs <- union(eqs, unlist0(deps_recursive[eqs]))
      used <- union(used, eqs)

      is_time <- stage[eqs] %in% c("time", "data")
      eqs_time <- intersect(names(equations), eqs[is_time])
      unpack <- intersect(variables, c(eqs, deps))
      required <- union(required, eqs[!is_time])

      if (phase %in% c("update", "deriv", "output")) {
        check <- c(e, equations[eqs_time])
        err <- lapply(check, function(eq) {
          intersect(data, eq$rhs$depends$variables)
        })
        is_err <- lengths(err) > 0
        if (any(is_err)) {
          data_err <- intersect(data, unlist0(err))
          src <- lapply(check[is_err], "[[", "src")
          odin_parse_error(
            c("Data may only be referenced from equations used in comparison",
              i = paste("You have referenced data {squote(data_err)} from",
                        "the '{phase}()' function or its dependencies, which",
                        "is not allowed because data are not defined",
                        "at this point")),
            "E2010", src, call)
        }
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
        eqs_data <- intersect(names(equations), eqs[is_time])
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


parse_storage <- function(equations, phases, variables, arrays, parameters,
                          data, call) {
  dim <- names(which(
    vlapply(equations, function(x) identical(x$special, "dim"))))
  shared <- setdiff(
    intersect(names(equations), phases$build_shared$equations),
    dim)
  stack <- setdiff(names(equations), c(shared, dim, arrays$name))
  internal <- intersect(phases$update$equations, arrays$name)

  packing <- list(state = parse_packing(variables, arrays, "state"))

  contents <- list(
    variables = variables,
    shared = shared,
    internal = internal,
    dim = dim,
    data = data$name,
    output = character(),
    stack = stack)
  location <- set_names(rep(names(contents), lengths(contents)),
                        unlist(contents, FALSE, TRUE))
  location[location == "variables"] <- "state"

  type <- set_names(rep("real_type", length(location)), names(location))
  type[parameters$name] <- parameters$type

  is_dim <- vlapply(equations[names(location)],
                    function(x) identical(x$special, "dim"))
  type[is_dim] <- "size_t"

  list(contents = contents,
       location = location,
       arrays = arrays,
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

  ## If time is continuous, we should also check that the reset
  ## variables don't reference any other variables, even indirectly;
  ## do this as mrc-5615.

  zero_every
}


parse_system_arrays <- function(exprs, call) {
  is_dim <- vlapply(exprs, function(x) identical(x$special, "dim"))

  dim_nms <- vcapply(exprs[is_dim], function(x) x$lhs$name_data)

  ## First, look for any array calls that do not have a corresponding
  ## dim()
  is_array <- !vlapply(exprs, function(x) is.null(x$lhs$array))
  err <- !vlapply(exprs[is_array], function(x) x$lhs$name %in% dim_nms)
  if (any(err)) {
    src <- exprs[is_array][err]
    err_nms <- unique(vcapply(src, function(x) x$lhs$name))
    odin_parse_error(
      paste("Missing 'dim()' for expression{?s} assigned as an array:",
            "{squote(err_nms)}"),
      "E2008", lapply(src, "[[", "src"), call)
  }

  ## Next, we collect up any subexpressions, in order, for all arrays,
  ## and make sure that we are always assigned as an array.
  nms <- vcapply(exprs, function(x) x$lhs$name %||% "") # empty for compare...
  for (nm in dim_nms) {
    i <- nms == nm & !is_dim
    err <- vlapply(exprs[i], function(x) {
      is.null(x$lhs$array) && !identical(x$special, "parameter")
    })
    if (any(err)) {
      src <- lapply(exprs[i][err], "[[", "src")
      odin_parse_error(
        c("Array expressions must always use '[]' on the lhs",
          i = paste("Your expression for '{nm}' has a 'dim()' equation, so it",
                    "is an array, but {cli::qty(sum(err))}",
                    "{?this usage/these usages} assign it as if it was a",
                    "scalar")),
        "E2009", src, call)
    }
  }

  ## TODO: This can be moved into the original parse of the
  ## expression, I think? We can revisit this later once the array
  ## bits have settled perhaps.
  is_array_assignment <- is_array | (nms %in% dim_nms)
  for (i in which(is_array_assignment)) {
    eq <- exprs[[i]]
    name_dim <- exprs[[which(is_dim)[[match(eq$lhs$name, dim_nms)]]]]$lhs$name
    eq$rhs$depends$variables <- union(eq$rhs$depends$variables, name_dim)
    exprs[[i]] <- eq
  }

  ## id <- vcapply(exprs, "[[", "id")
  ## if (anyDuplicated(id)) {
  ##   ## TODO: check here that all duplicates
  ##   for (i in unique(id[duplicated(id)])) {
  ##     j <- id == i
  ##     exprs[j] <- parse_system_arrays_check_duplicated(exprs[j], call)
  ##   }
  ## }

  ## Now, check combine any duplicates:
  ## special <- vcapply(exprs, function(x) x$special %||% "")
  ## name <- vcapply(exprs, function(x) x$lhs$name %||% "")
  ## key <- paste(special, name, sep = ":")
  ## if (any(duplicated(key))) {
  ##   for (nm in unique(key[duplicated(key)])) {
  ##     ## Here we'd be looking for everyone being array assignments
  ##     ## exprs <- parse_system_arrays_can_combine(key == nm, exprs, call)
  ##   }
  ## }

  exprs
}


parse_system_arrays_check_duplicated <- function(exprs, call) {
  is_array_assignment <- vlapply(exprs, function(eq) {
    eq$rhs$type == "expression" && !is.null(eq$lhs$array)
  })
  if (!all(is_array_assignment)) {
    stop("cope with this...")
  }

  index <- lapply(exprs, function(x) x$src$index)
  for (i in seq_along(index)[-1]) {
    if (index[[i]][[1]] != last(index[[i - 1]]) + 1) {
      stop("equations are not contiguous")
    }
  }

  for (i in seq_along(exprs)) {
    exprs[[i]]$id <- sprintf("%s:%d", exprs[[i]]$id, i)
  }

  exprs
}


parse_packing <- function(names, arrays, type) {
  scalar <- setdiff(names, arrays$name)
  if (length(scalar) > 0) {
    packing_scalar <- data_frame(
      name = scalar, rank = 0, dims = I(vector("list", length(scalar))),
      size = I(rep(list(1), length(scalar))))
    packing <- rbind(packing_scalar, arrays)
  } else {
    packing <- arrays
  }

  packing <- packing[match(names, packing$name), ]

  pack_group <- viapply(packing$size, function(x) {
    if (is.numeric(x)) 1L else 2L
  })
  packing <- packing[order(pack_group), ] # stable sort relative to names
  rownames(packing) <- NULL

  offset <- vector("list", nrow(packing))
  for (i in seq_along(offset)) {
    if (i == 1) {
      offset[[i]] <- 0
    } else {
      if (is.numeric(offset[[i - 1]])) {
        prev_offset <- offset[[i - 1]]
      } else {
        prev_offset <- call("OdinOffset", type, packing$name[[i - 1]])
      }
      if (is.numeric(packing$size[[i - 1]])) {
        prev_size <- packing$size[[i - 1]]
      } else {
        prev_size <- call("OdinLength", packing$name[[i - 1]])
      }
      offset[[i]] <- expr_plus(prev_offset, prev_size)
    }
  }
  packing$offset <- I(offset)

  packing
}
