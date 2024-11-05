odin_parse <- function(expr, input_type = NULL, compatibility = "warning") {
  call <- environment()
  odin_parse_quo(rlang::enquo(expr), input_type, compatibility, call)
}


odin_parse_quo <- function(quo, input_type, compatibility, call) {
  match_value(compatibility, c("silent", "warning", "error"), call = call)
  dat <- parse_prepare(quo, input_type, call)
  dat$exprs <- parse_compat(dat$exprs, compatibility, ignore_error = FALSE,
                            call = call)
  exprs <- lapply(dat$exprs, function(x) parse_expr(x$value, x, call = call))

  exprs <- parse_system_arrays(exprs, call)
  system <- parse_system_overall(exprs, call)
  equations <- parse_system_depends(
    system$exprs$equations, system$variables, call)
  phases <- parse_system_phases(
    system$exprs, equations, system$variables, system$parameters,
    system$data$name, call)
  storage <- parse_storage(
    equations, phases, system$variables, system$arrays, system$parameters,
    system$data, call)
  zero_every <- parse_zero_every(system$time, phases, equations,
                                 system$variables, call)
  print <- parse_print(system$exprs$print, system$time, system$variables,
                       system$data, phases, call)
  browser <- parse_browser(system$exprs$browser, system$time, system$variables,
                           system$data, phases, call)

  ret <- list(time = system$time,
              class = "odin",
              variables = system$variables,
              parameters = system$parameters,
              equations = equations,
              phases = phases,
              storage = storage,
              zero_every = zero_every,
              print = print,
              browser = browser,
              data = system$data)

  parse_check_usage(ret, call)
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

parse_check_consistent_dimensions_rhs <- function(eq, dat, call, src = eq$src) {
  throw_mismatch <- function(var, dim_rank, array_rank) {
    odin_parse_error(
      c("Array rank in expression differs from the rank declared with `dim`",
        i = paste("'{var}' has rank '{dim_rank}' in the `dim` call, ",
                  "but the line below assumes it has rank '{array_rank}'.")),
         "E2018", src, call)
  }

  throw_no_dim <- function(var) {
    odin_parse_error(
      paste("Missing 'dim()' for expression{?s} assigned as an array:",
            "{squote(var)}"),
      "E2008", src, call)
  }

  throw_non_array_arg <- function(func, var) {
    odin_parse_error(
      c("The function `{func}()` expects an array name without indexes.",
        i = "{var} is not a simple array name"),
      "E2019", src, call)
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
        lapply(expr[-(1:2)], check)

      } else if (rlang::is_call(expr, fn_use_whole_array)) {
        if (rlang::is_call(expr, "OdinReduce")) {
          array_name <- expr[[3]]
          if (!(array_name %in% names(dim_ranks))) {
            throw_no_dim(array_name)
          }
          index <- expr[[4]]
          if (!is.null(index)) {
            dim_rank <- dim_ranks[[array_name]]
            if (length(index) != dim_rank) {
              throw_mismatch(array_name, length(index), dim_rank)
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
    }
  }
  check(eq$rhs$expr)
}
