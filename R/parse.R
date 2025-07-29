odin_parse <- function(expr, input_type = NULL, compatibility = NULL,
                       check_bounds = NULL) {
  call <- environment()
  odin_parse_quo(rlang::enquo(expr), input_type, compatibility, check_bounds,
                 call)
}


odin_parse_quo <- function(quo, input_type, compatibility, check_bounds, call) {
  compatibility <- odin_compatibility_value(compatibility, call)
  check_bounds <- odin_check_bounds_value(check_bounds, call)
  dat <- parse_prepare(quo, input_type, call)
  dat$exprs <- parse_compat(dat$exprs, compatibility, ignore_error = FALSE,
                            call = call)
  exprs <- lapply(dat$exprs, function(x) parse_expr(x$value, x, call = call))

  exprs <- parse_system_arrays(exprs, call)
  system <- parse_system_overall(exprs, call)
  equations <- parse_system_depends(
    system$exprs$equations, system$ode_variables, call)
  equations <- parse_system_stage(
    equations, system$variables, system$parameters, system$data$name, call)
  delays <- parse_system_delays(
    equations, system$ode_variables, system$arrays, call)
  phases <- parse_system_phases(
    system$exprs, equations, system$variables, system$output,
    system$parameters, delays, system$data$name, call)

  if (!is.null(delays)) {
    delays$in_rhs <- delays$name %in% phases$deriv$equations
    delays$in_output <- delays$name %in% phases$output$equations
  }

  storage <- parse_storage(
    equations, phases, system$variables, system$output, system$arrays,
    system$parameters, system$data, delays, call)
  zero_every <- parse_zero_every(system$time, phases, equations,
                                 system$ode_variables, call)
  print <- parse_print(system$exprs$print, system$time, system$variables,
                       equations, system$data, phases, call)
  browser <- parse_browser(system$exprs$browser, system$time, system$variables,
                           system$data, phases, call)
  src <- lapply(exprs, "[[", "src")

  ret <- list(time = system$time,
              class = "odin_system",
              variables = system$variables,
              output = system$output,
              parameters = system$parameters,
              equations = equations,
              delays = delays,
              phases = phases,
              storage = storage,
              zero_every = zero_every,
              print = print,
              browser = browser,
              data = system$data,
              src = src)

  parse_check_usage(ret, call)
  parse_array_bounds(ret, check_bounds, call)
  ret <- parse_adjoint(ret)
  ret
}


parse_check_usage <- function(dat, call) {
  parse_check_usage_find_unknown(dat, call)
  parse_check_usage_find_unused(dat, call)
  parse_check_consistent_dimensions(dat, call)
}


parse_check_usage_find_unknown <- function(dat, call) {
  implicit <- c("time", if (dat$time == "discrete") "dt")
  built_in_constants <- "pi"
  known <- c(unlist(dat$storage$contents, FALSE, FALSE),
             implicit,
             built_in_constants,
             dat$storage$unused)
  eqs <- c(dat$phases$update$variables,
           dat$phases$deriv$variables,
           dat$phases$output$variables,
           dat$phases$initial$variables,
           dat$phases$compare$compare,
           unname(dat$equations))

  for (i in seq_along(eqs)) {
    eqs[[i]]$rhs$depends$variables <- lapply(eqs[[i]]$rhs$depends$variables,
      function(x) {
        if (length(x) > 0)  {
          is_dim <- grepl("^dim_", x)
          x[is_dim] <- substring(x[is_dim], 5)
        }
        x
      })
  }

  unknown <- lapply(eqs, function(eq) {
    used <- c(eq$rhs$depends$variables, eq$lhs$depends$variables)
    setdiff(used, known)
  })

  err <- lengths(unknown) > 0
  if (!any(err)) {
    return()
  }
  err_nms <- unique(unlist(unknown))
  src <- lapply(eqs[err], "[[", "src")
  if (dat$time == "continuous" && "dt" %in% err_nms) {
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


parse_check_usage_find_unused <- function(dat, call) {
  unused <- dat$storage$unused
  if (length(unused) == 0) {
    return()
  }

  ## Later, we'll offer other options than error!
  eqs <- dat$equations
  src <- unname(lapply(eqs[names(eqs) %in% unused], "[[", "src"))
  ## Drop 'dim_' etc here from the message as it's confusing
  unused <- grep("^(dim|interpolate)_", unused, value = TRUE, invert = TRUE)
  odin_parse_error(
    "Unused equation{?s}: {squote(unused)}",
    "E2016", src, call)
}

parse_check_consistent_dimensions <- function(dat, call) {
  eqs <- c(dat$phases$update$variables,
           dat$phases$deriv$variables,
           dat$phases$output$variables,
           dat$phases$initial$variables,
           dat$phases$compare$compare,
           unname(dat$equations))
  for (eq in eqs) {
    if (!is.null(eq$lhs)) {
      parse_check_consistent_dimensions_lhs(eq, dat, call)
    }
    if (!is.null(eq$rhs)) {
      parse_check_consistent_dimensions_rhs(eq, dat, call)
    }
    if (identical(eq$rhs$type, "compare")) {
      parse_check_consistent_dimensions_compare(eq, dat, call)
    }
  }
}

parse_check_consistent_dimensions_lhs <- function(eq, dat, call, src = eq$src) {
  throw_mismatch <- function(var, dim_rank, array_rank) {
    odin_parse_error(
      c("Array rank in expression differs from the rank declared with `dim`",
        i = paste("'{var}' has rank '{dim_rank}' in the `dim` call, ",
                  "but the line below assumes it has rank '{array_rank}'.")),
      "E2018", src, call)
  }

  check <- function(expr) {
    if (!is.null(expr$array)) {
      rank <- length(expr$array)
      dim_rank <- dat$storage$arrays$rank[dat$storage$arrays$name == expr$name]
      if (rank != dim_rank) {
        throw_mismatch(expr$name, dim_rank, rank)
      }
    }
  }
  check(eq$lhs)
}


parse_check_consistent_dimensions_expr <- function(expr, src, dat, call) {
  throw_mismatch <- function(var, dim_rank, array_rank) {
    odin_parse_error(
      c("Array rank in expression differs from the rank declared with `dim`",
        i = paste("'{var}' has rank '{dim_rank}' in the `dim` call, ",
                  "but the line below assumes it has rank '{array_rank}'.")),
         "E2018", src, call)
  }

  throw_no_dim <- function(var) {
    odin_parse_error(
      paste("Missing 'dim()' for expression{?s} used as an array:",
            "{squote(var)}"),
      "E2008", src, call)
  }

  throw_non_array_arg <- function(func, var) {
    odin_parse_error(
      c("The function `{func}()` expects an array name without indexes.",
        i = "{var} is not a simple array name"),
      "E2019", src, call)
  }

  throw_array_as_scalar <- function(var, rank) {
    what <- rank_description(rank)
    if (rank == 1) {
      dummy_index <- "..."
    } else {
      dummy_index <- paste(rep(".", rank), collapse = ", ")
    }
    odin_parse_error(
      c("Trying to use {what} '{var}' without index",
        i = sprintf("Did you mean '{var}[%s]'", dummy_index)),
      "E2022", src, call)
  }

  throw_empty_index_rhs <- function(name, expr) {
    odin_parse_error(
      c("Can't use an empty index while accessing arrays on the rhs",
        x = "In access of '{name}' as '{deparse(expr)}'"),
      "E2026", src, call)
  }

  throw_range_access_rhs <- function(name, expr) {
    odin_parse_error(
      c("Can't use the range operator `:` while accessing arrays on the rhs",
        x = "In access of '{name}' as '{deparse(expr)}'"),
      "E2026", src, call)
  }

  fn_use_whole_array <- c("length", "nrow", "ncol", "OdinReduce",
                          "OdinInterpolate")

  dim_ranks <- set_names(as.list(dat$storage$arrays$rank),
                         dat$storage$arrays$name)

  check <- function(expr) {
    if (is.recursive(expr)) {
      if (rlang::is_call(expr, "[")) {
        array_rank <- length(expr) - 2L
        array_name <- deparse(expr[[2]])
        is_dim_array <- array_name %in% names(dim_ranks)
        if (!is_dim_array) {
          throw_no_dim(array_name)
        } else {
          dim_rank <- dim_ranks[[array_name]]
          if (array_rank != dim_rank) {
            throw_mismatch(array_name, dim_rank, array_rank)
          }
        }
        args <- as.list(expr[-(1:2)])
        if (any(vlapply(args, rlang::is_missing))) {
          throw_empty_index_rhs(array_name, expr)
        }
        if (any(vlapply(args, rlang::is_call, ":"))) {
          throw_range_access_rhs(array_name, expr)
        }
        lapply(args, check)
      } else if (rlang::is_call(expr, fn_use_whole_array)) {
        if (rlang::is_call(expr, "OdinReduce")) {
          array_name <- expr$what
          if (!(array_name %in% names(dim_ranks))) {
            throw_no_dim(array_name)
          }
          index <- expr[[4]]
          if (!is.null(index)) {
            dim_rank <- dim_ranks[[array_name]]
            if (length(index) != dim_rank) {
              throw_mismatch(array_name, dim_rank, length(index))
            }
          }
        } else {
          func <- deparse(expr[[1]])
          arg <- expr[[2]]
          if (is.recursive(arg) || !is.symbol(arg)) {
            throw_non_array_arg(func, deparse(arg))
          }
          array_name <- deparse(arg)
          if (!(array_name %in% names(dim_ranks))) {
            throw_no_dim(array_name)
          }
        }
      } else {
        lapply(expr[-1], check)
      }
    } else if (is.symbol(expr)) {
      nm <- as.character(expr)
      if (nm %in% dat$storage$arrays$name) {
        throw_array_as_scalar(nm, dim_ranks[[nm]])
      }
    }
  }
  check(expr)
}

parse_check_consistent_dimensions_rhs <- function(eq, dat, call) {
  parse_check_consistent_dimensions_expr(eq$rhs$expr, eq$src, dat, call)
}


parse_check_consistent_dimensions_compare <- function(eq, dat, call) {
  lapply(eq$rhs$args, parse_check_consistent_dimensions_expr,
         eq$src, dat, call)
}


odin_compatibility_value <- function(compatibility, call) {
  if (is.null(compatibility)) {
    compatibility <- getOption("odin2.compatibility", "warning")
  }
  ## Slightly different list to below, we'll add "disabled" here soon
  valid <- c("warning", "silent", "error")
  match_value(compatibility, valid, call = call)
}


odin_check_bounds_value <- function(check_bounds, call) {
  if (is.null(check_bounds)) {
    check_bounds <- getOption("odin2.check_bounds", "error")
  }
  valid <- c("error", "warning", "disabled")
  match_value(check_bounds, valid, call = call)
}
