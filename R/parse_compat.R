parse_compat <- function(exprs, action, ignore_error, call) {
  apply_fix <- function(exprs, f) {
    if (ignore_error) {
      lapply(exprs, function(expr) {
        tryCatch(f(expr, call), odin_parse_error = function(err) {
          expr$error <- err
          expr
        })
      })
    } else {
      lapply(exprs, f, call)
    }
  }

  exprs <- apply_fix(exprs, parse_compat_fix_user)
  exprs <- apply_fix(exprs, parse_compat_fix_parameter_array)
  exprs <- apply_fix(exprs, parse_compat_fix_distribution)
  exprs <- apply_fix(exprs, parse_compat_fix_compare)

  ## Bunch of time-related things; these are a bit harder, and can't
  ## always be recovered from.
  exprs <- apply_fix(exprs, parse_compat_fix_assign_time)
  exprs <- apply_fix(exprs, parse_compat_fix_assign_dt)
  exprs <- apply_fix(exprs, parse_compat_fix_use_t)
  exprs <- apply_fix(exprs, parse_compat_fix_use_step)

  parse_compat_report(exprs, action, call)
}


parse_compat_fix_user <- function(expr, call) {
  is_user_assignment <-
    rlang::is_call(expr$value, c("<-", "=")) &&
    rlang::is_call(expr$value[[3]], "user")
  if (is_user_assignment) {
    res <- match_call(
      expr$value[[3]],
      function(default, integer, min, max, ...) NULL)
    if (!res$success) {
      odin_parse_error(
        "Failed to translate your 'user()' expression to use 'parameter()'",
        "E1016", expr, call = call)
    }
    for (arg in c("min", "max")) {
      if (!rlang::is_missing(res$value[[arg]])) {
        odin_parse_error(
          c("Can't yet translate 'user()' calls that use the '{arg}' argument",
            i = paste("We don't support this argument in parameter() yet,",
                      "but once we do we will support translation")),
          "E0001", expr, call = call)
      }
    }

    original <- expr$value
    args <- list(as.name("parameter"))
    if (!rlang::is_missing(res$value$default)) {
      args <- c(args, list(res$value$default))
    }
    if (!rlang::is_missing(res$value$integer)) {
      type <- if (isTRUE(res$value$integer)) "integer" else "real"
      args <- c(args, list(type = type))
    }
    expr$value[[3]] <- as.call(args)
    expr <- parse_add_compat(expr, "user", original)
  }
  expr
}


parse_compat_fix_parameter_array <- function(expr, call) {
  is_parameter_array_assignment <-
    rlang::is_call(expr$value, c("<-", "=")) &&
    rlang::is_call(expr$value[[3]], "parameter") &&
    rlang::is_call(expr$value[[2]], "[")

  if (is_parameter_array_assignment) {
    original <- expr$value
    expr$value[[2]] <- expr$value[[2]][[2]]
    expr <- parse_add_compat(expr, "parameter_array", original)
  }

  expr
}


parse_compat_fix_distribution <- function(expr, call) {
  ## for most (or all) of these we can do a direct translation, the
  ## args are unnamed.
  translate <- c(
    rbeta = quote(Beta),
    rbinom = quote(Binomial),
    rcauchy = quote(Cauchy),
    rchisq = quote(ChiSquared),
    rexp = quote(Exponential),
    rf = quote(F),
    rgamma = quote(Gamma),
    rgeometric = quote(Geometric),
    rhyper = quote(Hypergeometric),
    rlogis = quote(Logistic),
    rlnorm = quote(Lognormal),
    rnbinom = quote(NegativeBinomial),
    rnorm = quote(Normal),
    rpois = quote(Poisson),
    rt = quote(T),
    runif = quote(Uniform),
    rweibull = quote(Weibull),
    ## Nonstandard interface
    rmultinom = quote(Multinomial),
    rmhyper = quote(MultivariateHypergeometric))

  if (rlang::is_call(expr$value, c("<-", "="))) {
    if (any(names(translate) %in% all.names(expr$value))) {
      original <- expr$value
      expr$value <- substitute_(expr$value, translate)
      expr$compat <- list(type = "distribution", original = original)
    }
  }

  expr
}


parse_add_compat <- function(expr, type, original) {
  if (is.null(expr$compat)) {
    expr$compat <- list(type = type, original = original)
  } else {
    expr$compat$type <- c(expr$compat$type, type)
  }
  expr
}


parse_compat_report <- function(exprs, action, call) {
  i <- !vlapply(exprs, function(x) is.null(x$compat))
  if (action != "silent" && any(i)) {
    description <- c(
      user = "Replace calls to 'user()' with 'parameter()'",
      parameter_array =
        "Drop arrays from lhs of assignments from 'parameter()'",
      distribution = paste(
        "Replace calls to r-style random number calls (e.g., 'rnorm()')",
        "with monty-stye calls (e.g., 'Normal()')"),
      compare = paste(
        "Remove redundant 'compare()' wrapper, because all expressions",
        "using `~` are comparisons."),
      assign_time = paste(
        "Don't assign 'time' as 'step * dt', as this is now done",
        "automatically"),
      assign_dt = paste(
        "Don't assign 'dt', as this is provided on system creation"),
      use_step =
        "Don't use 'step', this no longer exists",
      use_t =
        "Use 'time' and not 't' to refer to time")

    type <- lapply(exprs[i], function(x) x$compat$type)
    err <- unlist0(type)

    detail <- NULL
    for (t in intersect(names(description), err)) {
      j <- which(i)[vlapply(type, function(x) t %in% x)]
      ## Getting line numbers here is really hard, so let's just not
      ## try for now and do this on deparsed expressions.
      updated <- vcapply(exprs[j], function(x) {
        if (is.null(x$value)) {
          "(delete this statement)"
        } else {
          deparse1(x$value)
        }
      })
      ## TODO: this should prefer to use the actual source, not the
      ## deparsed source, where possible.
      original <- vcapply(exprs[j], function(x) deparse1(x$compat$original))
      context_t <- set_names(
        c(rbind(original, updated, deparse.level = 0)),
        rep(c("x", "v"), length(updated)))
      detail <- c(detail, description[[t]], cli_nbsp(context_t))
    }

    header <- "Found {length(err)} compatibility issue{?s}"

    if (action == "error") {
      odin_parse_error(c(header, detail), "E1017", exprs[i], call)
    } else {
      data <- lapply(exprs[i], function(x) {
        x$compat$description <- description[[x$compat$type]]
        x
      })
      cli::cli_warn(c(header, detail),
                    class = "odin_compatibility_problem",
                    data = data,
                    call = call)
    }
  }

  i <- vlapply(exprs, function(x) !is.null(x$compat) && is.null(x$value))
  if (any(i)) {
    exprs <- exprs[!i]
  }

  exprs
}

parse_compat_fix_compare <- function(expr, call) {
  is_compare <-
    rlang::is_call(expr$value, "~") &&
    rlang::is_call(expr$value[[2]], "compare")
  if (is_compare) {
    original <- expr$value
    expr$value[[2]] <- expr$value[[2]][[2]]
    expr <- parse_add_compat(expr, "compare", original)
  }
  expr
}


parse_compat_fix_assign_time <- function(expr, call) {
  is_set_time <- rlang::is_call(expr$value, c("<-", "=")) &&
    identical(expr$value[[2]], quote(time))
  if (is_set_time) {
    rhs <- expr$value[[3]]
    is_time_from_step <- rlang::is_call(rhs, "*") && (
      (identical(rhs[[2]], quote(dt)) && identical(rhs[[3]], quote(step))) ||
      (identical(rhs[[3]], quote(dt)) && identical(rhs[[3]], quote(step))))
    if (is_time_from_step) {
      original <- expr$value
      expr$value <- NULL
      expr <- parse_add_compat(expr, "assign_time", original)
    } else {
      odin_parse_error(
        c("Don't assign to 'time'",
          i = paste(
            "Previously, in odin1, this was fine, but is now an error.",
            "Some models have used {.code time <- step * dt} or similar",
            "previously, but this is no longer necessary"),
          x = paste(
            "Your code contains an assignment to 'time' that we can't",
            "automatically migrate"),
          i = "Please see {.vignette migrating} for guidance"),
        "E1048", expr, call)
    }
  }
  expr
}


parse_compat_fix_assign_dt <- function(expr, call) {
  is_set_dt <- rlang::is_call(expr$value, c("<-", "=")) &&
    identical(expr$value[[2]], quote(dt))
  if (is_set_dt) {
    rhs <- expr$value[[3]]
    if (rlang::is_call(rhs, "parameter")) {
      original <- expr$value
      expr$value <- NULL
      expr <- parse_add_compat(expr, "assign_dt", original)
    } else {
      ## TODO: we can do better here once dust can cope with default
      ## dt values *and* once we work out how setting those from odin
      ## might look.  For now, this is not allowed.
      odin_parse_error(
        c("Don't assign to 'dt'",
          i = "'dt' is now provided to {.help dust::dust_system_create}'",
          x = paste(
            "Your odin code contains an assignment to 'dt' that we can't",
            "automatically migrate"),
          i = "Please see {.vignette migrating} for guidance"),
        "E1049", expr, call)
    }
  }
  expr
}


parse_compat_fix_use_t <- function(expr, call) {
  deps <- all.vars(expr$value)
  if ("t" %in% deps) {
    original <- expr$value
    expr$value <- substitute_(expr$value, list(t = quote(time)))
    expr <- parse_add_compat(expr, "use_t", original)
  }
  expr
}


parse_compat_fix_use_step <- function(expr, call) {
  deps <- all.vars(expr$value)
  if ("step" %in% deps) {
    ## We could consider how accesses here look and point people in
    ## the right direction, but the docs also do this.
    odin_parse_error(
      c("Use of 'step' is no longer allowed",
        i = paste("Previously, discrete-time models used 'step' as a measure",
                  "of time, but we have removed this in odin2"),
        i = "Please see {.vignette migrating} for guidance"),
      "E1050", expr, call)
  }
  expr
}
