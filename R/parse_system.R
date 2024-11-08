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
  is_print <- special == "print"
  is_browser <- special == "browser"
  is_equation <- special %in% c("", "parameter", "dim") & !is_compare

  ## We take initial as the set of variables:
  variables <- unique(vcapply(exprs[is_initial], function(x) x$lhs$name))
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

  if (any(is_output)) {
    if (!is_continuous) {
      odin_parse_error(
        c("Can't use 'output()' in discrete time systems",
          i = paste("You should be able to do what you need using 'update()'.",
                    "If you are migrating from odin 1.x.x, you might find",
                    'some advice in {.vignette "migrating"}')),
        "E2020", src, call)
    }

    output <- unique(vcapply(exprs[is_output], function(x) x$lhs$name))
    variables <- c(variables, output)
  } else {
    output <- NULL
  }

  dims <- lapply(exprs[is_dim], function(x) x$rhs$value)
  arrays <- data_frame(
    name = vcapply(exprs[is_dim], function(x) x$lhs$name_data),
    rank = lengths(dims),
    dims = I(dims),
    size = I(lapply(dims, expr_prod)))

  parameters <- parse_system_overall_parameters(exprs, arrays)
  ## This now means that our parameters are nolonger the source of
  ## truth.

  data <- data_frame(
    name = vcapply(exprs[is_data], function(x) x$lhs$name))

  ## This whole chunk probably moves into its own section at some
  ## point.
  is_interpolate <- vlapply(exprs,
                            function(x) identical(x$rhs$type, "interpolate"))
  if (any(is_interpolate)) {
    exprs[is_interpolate] <- lapply(exprs[is_interpolate], function(eq) {
      nm_result <- eq$lhs$name_data
      nm_time <- eq$rhs$expr$time
      nm_value <- eq$rhs$expr$value

      get_rank <- function(nm) {
        if (is.na(i <- match(nm, arrays$name))) 0L else arrays$rank[[i]]
      }
      rank_result <- get_rank(nm_result)

      if (get_rank(nm_time) != 1L) {
        odin_parse_error(
          c(paste("Expected time argument '{nm_time}' to 'interpolate()' for",
                  "'{nm_result}' to be a vector"),
            i = "{nm_time} was a {rank_description(get_rank(nm_time))}"),
          "E2015", eq$src, call)
      }
      rank_value_expected <- 1L + rank_result
      if (get_rank(nm_value) != rank_value_expected) {
        odin_parse_error(
          c(paste("Expected value argument '{nm_value}' to 'interpolate()' for",
                  "'{nm_result}' to be a",
                  "{rank_description(rank_value_expected)}"),
            i = "{nm_value} was a {rank_description(get_rank(nm_value))}"),
          "E2015", eq$src, call)
      }

      ## TODO: we could warn here about things that look incompatible,
      ## but a lot of the time that's hard to tell, and particularly
      ## so with arrays whose size is not known until they are given.

      ## Finally, we save the rank back in, and the source of the rank
      ## information.
      eq$rhs$expr$rank <- rank_result
      if (rank_result > 0) {
        eq$rhs$expr$dim <- eq$lhs$name_data
      }
      eq
    })
    interpolate_use <- lapply(
      exprs[is_interpolate],
      function(eq) {
        list(
          lhs = list(name = eq$lhs$name_data),
          rhs = list(type = "expression",
                     expr = call("OdinInterpolateEval",
                                 eq$lhs$name, eq$lhs$name_data),
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
                print = exprs[is_print],
                browser = exprs[is_browser],
                data = exprs[is_data])

  nms <- vcapply(exprs$equations, function(x) x$lhs$name)
  err <- intersect(nms, variables)
  if (length(err) > 0) {
    src <- lapply(exprs$equations[nms %in% err], "[[", "src")
    odin_parse_error(
      paste("{?Equation uses name/Equations use names} belonging to",
            "variable{?s}: {squote(err)}"),
      "E2014", src, call)
  }

  list(time = if (is_continuous) "continuous" else "discrete",
       variables = variables,
       output = output,
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
    setdiff(unique(c(eq$lhs$depends$variables, eq$rhs$depends$variables)),
            c(implicit, eq$lhs$name))
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
    setdiff(unique(c(eq$lhs$depends$variables, eq$rhs$depends$variables)),
            automatic)
  })
  deps_recursive <- list()
  for (i in seq_along(deps)) {
    nm <- names(deps)[[i]]
    vars <- deps[[i]]
    deps_recursive[[nm]] <- unique(c( # union, but for 3 args...
      deps_recursive[[nm]],
      vars,
      unlist(deps_recursive[vars], FALSE, FALSE)))
  }

  for (nm in names(deps_recursive)) {
    for (i in which(names(equations) == nm)) {
      equations[[i]]$rhs$depends$variables_recursive <- deps_recursive[[nm]]
    }
  }

  equations
}


## Next step, make phase (again) a property of a name, not an id.  I
## don't think the alternative is interesting enough to warrant the
## effort.  Then things simplify back out quite nicely again....
parse_system_phases <- function(exprs, equations, variables, parameters, data,
                                call) {
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
    eq <- equations[[i]]
    rhs <- eq$rhs
    vars <- setdiff(rhs$depends[["variables"]], eq$lhs$name)
    if (identical(rhs$type, "parameter")) {
      is_constant <- isTRUE(
        parameters$constant[match(eq$lhs$name, parameters$name)])
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
  is_err <- stage_dim != "system_create"
  if (any(is_err)) {
    err <- equations[is_dim][is_err]
    err_nms <- vcapply(err, function(x) x$lhs$name_data)
    err_stage <- stage_dim[is_err]
    err_deps <- vcapply(err, function(x) {
      deps <- x$rhs$depends$variables_recursive
      stage_deps <- stage[deps]
      stage_deps[is.na(stage_deps)] <- "unknown!"
      paste(sprintf("'%s' (%s)", deps, stage_deps), collapse = ", ")
    })
    detail <- sprintf(
      "'%s' is determined at stage '%s', it depends on %s",
      err_nms, err_stage, err_deps)
    src <- unname(lapply(err, "[[", "src"))
    hint <- NULL

    if (any(err_stage == "parameter_update")) {
      deps <- unlist0(lapply(err[err_stage == "parameter_update"],
                             function(x) x$rhs$depends$variables_recursive))
      deps_pars <- deps[vlapply(equations[deps], function(eq) {
        identical(eq$special, "parameter") &&
          eq$lhs$name %in% names(stage) &&
          stage[[eq$lhs$name]] == "parameter_update"
      })]
      if (length(deps_pars) > 0) {
        hint <- paste(
          "Try adding {.code constant = TRUE} into the 'parameter()' call{?s}",
          "for {squote(deps_pars)}")
      }
    }
    odin_parse_error(
      c("Dimensions of arrays are not determined at initial creation",
        set_names(detail, "x"),
        set_names(hint, "i")),
      "E2011", src, call)
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
        check <- c(e, unname(equations[eqs_time]))
        err <- lapply(check, function(eq) {
          intersect(data, eq$rhs$depends$variables)
        })
        is_err <- lengths(err) > 0
        if (any(is_err)) {
          data_err <- intersect(data, unlist0(err))
          src <- unname(lapply(check[is_err], "[[", "src"))
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


parse_storage <- function(equations, phases, variables, output, arrays,
                          parameters, data, call) {
  used <- unique(unlist0(lapply(phases, "[[", "equations")))
  unused <- setdiff(names(equations), used)

  dim <- names(which(
    vlapply(equations[used], function(x) identical(x$special, "dim"))))
  shared <- setdiff(
    intersect(used, phases$build_shared$equations),
    dim)
  stack <- setdiff(used, c(shared, dim, arrays$name))
  internal <- setdiff(intersect(used, arrays$name), shared)

  packing <- list(
    state = parse_packing(variables, arrays, output, "state"))

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

  storage_type <- vcapply(equations,
                          function(eq) eq$lhs$storage_type %||% NA_character_)
  i <- !is.na(storage_type)
  if (any(i)) {
    stopifnot(!anyDuplicated(names(storage_type[i]))) # array corner case
    type[names(storage_type[i])] <- storage_type[i]
  }

  is_interpolate <- vlapply(equations[names(location)],
                            function(x) identical(x$rhs$type, "interpolate"))
  type[names(type) %in% names(which(is_interpolate))] <- "interpolator"
  type[dim] <- "dimension"

  list(contents = contents,
       location = location,
       arrays = arrays,
       type = type,
       unused = unused,
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
                    "{?this usage assigns/these usages assign} it as if it were a",
                    "scalar")),
        "E2009", src, call)
    }
  }


  name_dim_equation <- set_names(
    vcapply(exprs[is_dim], function(eq) eq$lhs$name),
    dim_nms)

  is_array_assignment <- is_array | (nms %in% dim_nms)
  for (i in which(is_array_assignment)) {
    eq <- exprs[[i]]
    if (eq$rhs$type == "parameter" && !is.null(eq$rhs$args$default)) {
      odin_parse_error(
        "Array parameters cannot have defaults",
        "E1051", eq$src, call)
    }
    eq$rhs$depends$variables <- union(eq$rhs$depends$variables,
                                      name_dim_equation[[eq$lhs$name]])
    exprs[[i]] <- eq
  }

  is_interpolate <- vlapply(exprs, function(eq) eq$rhs$type == "interpolate")
  for (i in which(is_interpolate)) {
    eq <- exprs[[i]]
    if (eq$lhs$name_data %in% dim_nms) {
      eq$rhs$depends$variables <- union(
        eq$rhs$depends$variables, name_dim_equation[[eq$lhs$name_data]])
      exprs[[i]] <- eq
    }
  }

  id <- sprintf("%s:%s", vcapply(exprs, function(x) x$special %||% ""), nms)
  ## TODO: Fix this properly in mrc-5867
  is_compare <- vlapply(exprs, function(x) identical(x$rhs$type, "compare"))
  if (any(is_compare)) {
    id[is_compare] <- paste0(
      "compare:",
      vcapply(exprs[is_compare], function(x) as.character(x$rhs$args[[1]])))
  }

  if (anyDuplicated(id)) {
    for (i in unique(id[duplicated(id)])) {
      parse_system_arrays_check_duplicated(id == i, exprs, call)
    }
  }

  exprs
}


parse_system_arrays_check_duplicated <- function(i, exprs, call) {
  index <- which(i)
  nm <- exprs[[index[[1]]]]$lhs$name

  is_array_assignment <- vlapply(exprs[i], function(eq) {
    eq$rhs$type == "expression" && !is.null(eq$lhs$array)
  })
  if (!all(is_array_assignment)) {
    src <- lapply(exprs[i], "[[", "src")
    odin_parse_error(
      paste("Only arrays can be assigned over multiple statements, but",
            "'{nm}' is assigned as a symbol"),
      "E2012", src, call)
  }

  if (any(diff(index) > 1)) {
    index_others <- Filter(
      function(j) j > index[[1]] && j < last(index),
      which(!i))
    others <- unique(vcapply(exprs[index_others], function(x) x$lhs$name))
    src <- lapply(exprs[seq(index[[1]], last(index))], "[[", "src")
    odin_parse_error(
      paste("Multiline array equations must be contiguous",
            "statements, but '{nm}' is interleaved with {squote(others)}"),
      "E2013", src, call)
  }
}


parse_packing <- function(names, arrays, no_reorder, type) {
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
  pack_first <- vlapply(packing$size, is.numeric) &
    !(packing$name %in% no_reorder)
  packing <- packing[order(!pack_first), ]
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


parse_print <- function(print, time_type, variables, data, phases, call) {
  if (length(print) == 0) {
    return(NULL)
  }

  phase <- vcapply(print, function(eq) {
    deps <- eq$depends$variables
    if (any(data$name)) {
      ## This is not actually enough - it's also all recursive deps
      ## of data...
      odin_parse_error("Can't yet reference data from 'print()'",
                       "E0001", eq$src, call)
    }
    if (time_type == "discrete") "update" else "deriv"
  })

  deps <- lapply(print, function(x) x$depends$variables)

  ret <- list()
  for (p in phase) {
    i <- phase == p
    unpack <- setdiff(intersect(unlist0(deps[i]), variables),
                      phases[[p]]$unpack)
    ret[[p]] <- list(unpack = unpack, equations = print[i])
  }

  ret
}


parse_browser <- function(browser, time_type, variables, data, phases, call) {
  if (length(browser) == 0) {
    return(NULL)
  }

  phase <- vcapply(browser, "[[", "phase")
  if (anyDuplicated(phase)) {
    stop("duplicated phase")
  }

  names(browser) <- phase
  ret <- list()

  for (p in phase) {
    if (is.null(phases[[p]])) {
      valid <- intersect(names(phases)[!vlapply(phases, is.null)],
                         PHASES_BROWSER)
      src <- browser[[p]]$src
      odin_parse_error(
        c(paste("Cannot use 'browser()' with phase '{p}', as it does",
                "not exist in your system"),
          i = "Valid choices are: {squote(valid)}"),
        "E2017", src, call)
    }

    unpack <- setdiff(variables, phases[[p]]$unpack)
    ret[[p]] <- list(phase = p,
                     when = browser[[p]]$when,
                     unpack = unpack)
  }

  ret
}


parse_system_overall_parameters <- function(exprs, arrays) {
  special <- vcapply(exprs, function(x) x$special %||% "")
  dims <- exprs[is_dim <- special == "dim"]
  pars <- exprs[special == "parameter"]

  ## Find direct uses of parameters within dim expressions.  Change
  ## the interpretation of default for type and constant based on
  ## these.
  used_directly_as_dims <- unique(unlist0(lapply(dims, function(eq) {
    eq$rhs$depends$variables
  })))
  is_differentiable <- vlapply(pars, function(x) x$rhs$args$differentiate)

  pars <- lapply(pars, function(eq) {
    is_used_directly_as_dim <- eq$lhs$name %in% used_directly_as_dims
    if (is.na(eq$rhs$args$constant)) {
      eq$rhs$args$constant <- is_used_directly_as_dim || any(is_differentiable)
    }
    if (is.na(eq$rhs$args$type)) {
      eq$rhs$args$type <- if (is_used_directly_as_dim) "int" else "real_type"
    }
    eq
  })

  is_constant <- vlapply(pars, function(x) x$rhs$args$constant)
  type <- vcapply(pars, function(x) x$rhs$args$type)
  required <- vlapply(pars,
                      function(x) is.null(x$rhs$args$default))

  name <- vcapply(pars, function(x) x$lhs$name)
  rank <- arrays$rank[match(name, arrays$name)]
  rank[is.na(rank)] <- 0L

  data_frame(
    name = name,
    type = type,
    rank = rank,
    required = required,
    differentiate = is_differentiable,
    constant = is_constant)
}
