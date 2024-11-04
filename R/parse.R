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

parse_check_consistent_dimensions_lhs <- function(expr, dat, call) {
  check <- function(expr, src) {
    if (!is.null(expr$array)) {
      rank <- length(expr$array)
      dim_rank <- dat$storage$arrays$rank[dat$storage$arrays$name == expr$name]
      if (rank != dim_rank) {
        odin_parse_error(
          c("Array rank in expression differs from the rank declared with `dim`",
          i = paste("'{expr$name}' has rank '{dim_rank}' in the `dim` call, ",
                    "but the line below assumes it has rank '{rank}'.")),
          "E2018", src, call)
      }
    }
  }
  check(expr$lhs, expr$src)
}

parse_check_consistent_dimensions_rhs <- function(expr, dat, call) {
  
  fn_use_whole_array <- c("length", "nrow", "ncol", "OdinReduce",
                          "OdinLength", "OdinInterpolate")
  
  arrays <- set_names(as.list(dat$storage$arrays$rank),
                      dat$storage$arrays$name)
  
  check <- function(expr, src) {
    if (is.recursive(expr)) {
      if (rlang::is_call(expr, "[")) {
        array_rank <- length(expr) - 2L
        array_name <- as.character(expr[[2]])
        is_dim_array <- array_name %in% names(arrays)
        if (!is_dim_array) {
          stop()
          # Array on RHS, no matching dim
        } else {
          if (array_rank != arrays[[array_name]]) {
            stop()
            # Dim rank doesn't match usage
          }
        }
        lapply(expr[-(1:2)], check, src)

      } else if (rlang::is_call(expr, fn_use_whole_array)) {
        if (rlang::is_call(expr, "OdinReduce")) {
          # Index 2 is "sum", 3 is "a", 4 is NULL or
          if (!expr[[3]] %in% names(arrays)) {
            # OdinReduce variable not found in arrays
            stop()
          }
          index <- expr[[4]]
          if (!is.null(index)) {
            if (length(index) != arrays[[expr[[3]]]]) {
                # OdinReduce call using incorrect rank
              stop()
            }
          }
        } else {
          browser()
        } 
      } else {
        # Assumed to be a function.
        lapply(expr[-1], check, src)
      }
    }
  }
  check(expr$rhs$expr, expr$src)
}


#    ranks <- find_ranks_in_expr(eq$rhs$expr)
#    arrays <- names(ranks)
#    for (i in seq_along(ranks)) {
#      dim_rank <- dat$storage$arrays$rank[dat$storage$arrays$name == arrays[i]]
#      if (dim_rank != ranks[i]) {
#        odin_parse_error(
#          c("Array rank in expression differs from the rank declared with `dim`",
#            i = paste("'{eq$lhs$name}' has rank '{dim_rank}' in the `dim` call, ",
#                      "but the line below assumes it has rank '{ranks[i]}'.")),
#          "E2018", eq$src, call)
#      }
#    }
#  }
# }