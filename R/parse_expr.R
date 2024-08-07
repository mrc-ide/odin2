parse_expr <- function(expr, src, call) {
  if (rlang::is_call(expr, c("<-", "="))) {
    parse_expr_assignment(expr, src, call)
  } else if (rlang::is_call(expr, "~")) {
    parse_expr_compare(expr, src, call)
  } else if (rlang::is_call(expr, "print")) {
    parse_expr_print(expr, src, call)
  } else {
    odin_parse_error(
      c("Unclassifiable expression",
        i = "Expected an assignment (with '<-') or a relationship (with '~')"),
      "E1001", src, call)
  }
}


parse_expr_assignment <- function(expr, src, call) {
  lhs <- parse_expr_assignment_lhs(expr[[2]], src, call)
  rhs <- parse_expr_assignment_rhs(expr[[3]], src, call)

  special <- lhs$special
  lhs$special <- NULL
  if (rhs$type == "data") {
    if (!is.null(special)) {
      odin_parse_error(
        "Calls to 'data()' must be assigned to a symbol",
        "E1002", src, call)
    }
    special <- "data"
  } else if (rhs$type == "parameter") {
    if (!is.null(special)) {
      odin_parse_error(
        "Calls to 'parameter()' must be assigned to a symbol",
        "E1014", src, call)
    }
    special <- "parameter"
  }

  if (identical(special, "initial")) {
    zero_every <- lhs$args$zero_every
    if (!rlang::is_missing(zero_every) && !is.null(zero_every)) {
      if (!rlang::is_integerish(zero_every)) {
        odin_parse_error(
          "Argument to 'zero_every' must be an integer",
          "E1019", src, call)
      }
      if (!(identical(rhs$expr, 0) || identical(rhs$expr, 0L))) {
        odin_parse_error(
          "Initial condition of periodically zeroed variable must be 0",
          "E1020", src, call)
      }
    }
  }

  list(special = special,
       lhs = lhs,
       rhs = rhs,
       src = src)
}


parse_expr_assignment_lhs <- function(lhs, src, call) {
  array <- NULL
  special <- NULL
  name <- NULL

  special_def <- list(
    initial = function(name, zero_every) NULL,
    update = function(name) NULL,
    deriv = function(name) NULL,
    output = function(name) NULL,
    dim = function(name) NULL,
    config = function(name) NULL,
    compare = function(name) NULL)

  args <- NULL
  if (rlang::is_call(lhs, names(special_def))) {
    special <- deparse1(lhs[[1]])
    m <- match_call(lhs, special_def[[special]])
    if (!m$success) {
      odin_parse_error(c("Invalid special function call",
                         x = conditionMessage(m$error)),
        "E1003", src, call)
    }
    if (rlang::is_missing(m$value$name)) {
      odin_parse_error(
        c("Invalid special function call",
          i = paste("Missing target for '{special}()', typically the first",
                    "(unnamed) argument")),
        "E1003", src, call)
    }

    if (special == "compare") {
      ## TODO: a good candidate for pointing at the source location of
      ## the error.
      odin_parse_error(
        c("'compare()' expressions must use '~', not '<-'",
          i = paste("Compare expressions do not represent assignents, but",
                    "relationships, which we emphasise by using '~'.  This",
                    "also keeps the syntax close to that for the prior",
                    "specification in mcstate2")),
        "E1004", src, call)
    }
    lhs <- lhs[[2]]
    if (length(m$value) > 2) {
      args <- as.list(m$value[-(1:2)])
      i <- !vlapply(args, rlang::is_missing)
      args <- args[i]
      if (length(args) == 0) {
        args <- NULL
      }
    }
  }

  is_array <- rlang::is_call(lhs, "[")
  if (is_array) {
    odin_parse_error(
      "Arrays are not supported yet", "E0001", src, call)
  }

  name <- parse_expr_check_lhs_name(lhs, src, call)

  lhs <- list(
    name = name,
    special = special)

  if (!is.null(args)) {
    lhs$args <- args
  }

  lhs
}


parse_expr_assignment_rhs <- function(rhs, src, call) {
  if (rlang::is_call(rhs, "delay")) {
    odin_parse_error("'delay()' is not implemented yet",
                     "E0001", src, call)
  } else if (rlang::is_call(rhs, "parameter")) {
    parse_expr_assignment_rhs_parameter(rhs, src, call)
  } else if (rlang::is_call(rhs, "data")) {
    parse_expr_assignment_rhs_data(rhs, src, call)
  } else if (rlang::is_call(rhs, "interpolate")) {
    odin_parse_error("'interpolate()' is not implemented yet",
                     "E0001", src, call)
  } else {
    parse_expr_assignment_rhs_expression(rhs, src, call)
  }
}


parse_expr_assignment_rhs_expression <- function(rhs, src, call) {
  ## So here, we want to find dependencies used in the rhs and make
  ## sure that the user correctly restricts to the right set of
  ## dependencies.  There is some faff with sum, and we detect here if
  ## the user uses anything stochastic.  We do look for the range
  ## operator but I'm not totally sure that's the best place to do so.
  depends <- find_dependencies(rhs)

  ## TODO: we're going to check usage in a couple of places, but this
  ## is the first pass.
  rhs <- parse_expr_usage(rhs, src, call)
  is_stochastic <- any(
    depends$functions %in% mcstate2::mcstate_dsl_distributions()$name)

  list(type = "expression",
       expr = rhs,
       is_stochastic = is_stochastic,
       depends = depends)
}


parse_expr_check_lhs_name <- function(lhs, src, call) {
  ## There are lots of checks we should add here, but fundamentally
  ## it's a case of making sure that we have been given a symbol and
  ## that symbol is not anything reserved, nor does it start with
  ## anything reserved.  Add these in later, see
  ## "ir_parse_expr_check_lhs_name" for details.
  if (!rlang::is_symbol(lhs)) {
    odin_parse_error("Expected a symbol on the lhs", "E1005", src, call)
  }
  name <- deparse1(lhs)
  name
}


## TODO: we'll have a variant of this that acts as a compatibility
## layer for user(), as this is probably the biggest required change
## to people's code, really.
parse_expr_assignment_rhs_parameter <- function(rhs, src, call) {
  template <- function(default = NULL, constant = NULL, differentiate = FALSE) {
  }
  result <- match_call(rhs, template)
  if (!result$success) {
    odin_parse_error(
      c("Invalid call to 'parameter()'",
        x = conditionMessage(result$error)),
      "E1006", src, call)
  }
  ## TODO: also error if any unnamed argument is not `default`
  args <- as.list(result$value)[-1]
  if (is.language(args$default)) {
    deps <- find_dependencies(args$default)
    if (length(deps$variables) > 0) {
      default_str <- deparse1(args$default)
      odin_parse_error(
        c("Invalid default argument to 'parameter()': {default_str}",
          i = paste("Default arguments can only perform basic arithmetic",
                    "operations on numbers, and may not reference any",
                    "other parameter or variable")),
        "E1007", src, call)
    }
    ## TODO: validate the functions used at some point, once we do
    ## that generally.
  }

  if (!is_scalar_logical(args$differentiate)) {
    str <- deparse1(args$differentiate)
    odin_parse_error(
      "'differentiate' must be a scalar logical, but was '{str}'",
      "E1008", src, call)
  }
  ## constant has a different default
  if (is.null(args$constant)) {
    args$constant <- NA
  } else if (!is_scalar_logical(args$constant)) {
    str <- deparse1(args$constant)
    odin_parse_error(
      "'constant' must be a scalar logical if given, but was '{str}'",
      "E1009", src, call)
  }

  if (args$differentiate && isTRUE(args$constant)) {
    odin_parse_error(
      "Differentiable parameters must not be constant",
      "E1015", src, call)
  }

  list(type = "parameter",
       args = args)
}


parse_expr_assignment_rhs_data <- function(rhs, src, call) {
  if (length(rhs) != 1) {
    odin_parse_error("Calls to 'data()' must have no arguments",
                     "E1010", src, call)
  }
  list(type = "data")
}


parse_expr_compare <- function(expr, src, call) {
  lhs <- parse_expr_compare_lhs(expr[[2]], src, call)
  rhs <- parse_expr_compare_rhs(expr[[3]], src, call)

  rhs$args <- c(lhs, rhs$args)
  rhs$depends$variables <- union(rhs$depends$variables, as.character(lhs))
  rhs$density$expr <- substitute_(
    rhs$density$expr,
    list2env(set_names(rhs$args, rhs$density$args)))
  list(special = "compare",
       rhs = rhs,
       src = src)
}


parse_expr_compare_lhs <- function(lhs, src, call) {
  if (!rlang::is_call(lhs, "compare")) {
    ## TODO: this is a good candidate for pointing at the assignment
    ## symbol in the error message, if we have access to the source,
    ## as that's the most likely fix.
    odin_parse_error(
      c("Expected the lhs of '~' to be a 'compare()' call",
        i = "Did you mean to use '<-' in place of '~'?"),
      "E1011", src, call)
  }
  lhs <- lhs[[2]]
  if (!is.symbol(lhs)) {
    odin_parse_error(
      "Expected the argument of 'compare()' to be a symbol",
      "E1012", src, call)
  }
  lhs
}


parse_expr_compare_rhs <- function(rhs, src, call) {
  result <- mcstate2::mcstate_dsl_parse_distribution(rhs, "The rhs of '~'")
  if (!result$success) {
    odin_parse_error(
      result$error,
      "E1013", src, call)
  }
  depends <- find_dependencies(rhs)
  density <- list(cpp = result$value$cpp$density,
                  expr = result$value$expr$density,
                  args = names(formals(result$value$density)))

  list(type = "compare",
       density = density,
       args = result$value$args,
       depends = depends)
}


parse_expr_print <- function(expr, src, call) {
  odin_parse_error(
    "'print()' is not implemented yet", "E0001", src, call)
}


parse_expr_usage <- function(expr, src, call) {
  if (is.recursive(expr)) {
    fn <- expr[[1]]
    fn_str <- as.character(fn)
    if (fn_str %in% mcstate2::mcstate_dsl_distributions()$name) {
      expr <- parse_expr_usage_rewrite_stochastic(expr, src, call)
    } else {
      args <- lapply(expr[-1], parse_expr_usage, src, call)
      expr <- as.call(c(list(fn), args))
    }
  }
  expr
}


parse_expr_usage_rewrite_stochastic <- function(expr, src, call) {
  res <- mcstate2::mcstate_dsl_parse_distribution(expr)
  if (!res$success) {
    odin_parse_error(res$error, "E1018", src, call)
  }

  ## Take the expectation here, in case we need to differentiate
  ## later
  args <- lapply(res$value$args, parse_expr_usage, src, call)
  mean <- substitute_(
    res$value$expr$mean,
    set_names(lapply(args, rewrite_stochastic_to_expectation),
              names(formals(res$value$sample))[-1]))

  expr[[1]] <- call("OdinStochasticCall",
                    sample = res$value$cpp$sample,
                    density = res$value$cpp$density,
                    mean = mean)
  expr[-1] <- args
  expr
}


rewrite_stochastic_to_expectation <- function(expr) {
  if (is.recursive(expr)) {
    if (rlang::is_call(expr[[1]], "OdinStochasticCall")) {
      expr[[1]]$mean
    } else {
      expr[-1] <- lapply(expr[-1], rewrite_stochastic_to_expectation)
      expr
    }
  } else {
    expr
  }
}
