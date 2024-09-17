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
  special <- lhs$special
  lhs$special <- NULL

  if (identical(special, "dim")) {
    lhs$name_data <- lhs$name
    lhs$name <- paste0("dim_", lhs$name)
    rhs <- parse_expr_assignment_rhs_dim(expr[[3]], src, call)
  } else {
    rhs <- parse_expr_assignment_rhs(expr[[3]], src, call)
  }

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
  } else if (rhs$type == "interpolate") {
    if (!is.null(special)) {
      odin_parse_error(
        "Calls to 'interpolate()' must be assigned to a symbol",
        "E1099", src, call)
    }
    lhs$name_data <- lhs$name
    lhs$name <- paste0("interpolate_", lhs$name)
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

  index_used <- intersect(INDEX, rhs$depends$variables)
  if (length(index_used) > 0) {
    n <- length(lhs$array)
    err <- intersect(index_used, INDEX[-seq_len(n)])
    if (length(err) > 0) {
      v <- err[length(err)]
      i <- match(v, INDEX)
      odin_parse_error(
        c("Invalid index access used on rhs of equation: {squote(err)}",
          i = paste("Your lhs has only {n} dimension{?s}, but index '{v}'",
                    "would require {match(v, INDEX)}")),
        "E1021", src, call)
    }
    ## index variables are not real dependencies, so remove them:
    rhs$depends$variables <- setdiff(rhs$depends$variables, INDEX)
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
    config = function(name) NULL)

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

  if (rlang::is_call(lhs, "[")) {
    name <- parse_expr_check_lhs_name(lhs[[2]], src, call)
    array <- Map(parse_expr_check_lhs_index,
                 seq_len(length(lhs) - 2),
                 lhs[-(1:2)],
                 MoreArgs = list(name = name, src = src, call = call))
  } else {
    name <- parse_expr_check_lhs_name(lhs, src, call)
    array <- NULL
  }

  lhs <- list(
    name = name,
    special = special,
    array = array)

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
    parse_expr_assignment_rhs_interpolate(rhs, src, call)
  } else {
    parse_expr_assignment_rhs_expression(rhs, src, call)
  }
}


parse_expr_assignment_rhs_expression <- function(rhs, src, call) {
  depends <- find_dependencies(rhs)

  ## TODO: we're going to check usage in a couple of places, but this
  ## is the first pass.
  rhs <- parse_expr_usage(rhs, src, call)
  is_stochastic <- any(
    depends$functions %in% monty::monty_dsl_distributions()$name)

  list(type = "expression",
       expr = rhs,
       is_stochastic = is_stochastic,
       depends = depends)
}


parse_expr_assignment_rhs_dim <- function(rhs, src, call) {
  if (rlang::is_call(rhs, "c")) {
    value <- as.list(rhs[-1])
  } else {
    value <- list(rhs)
  }
  depends <- join_dependencies(lapply(value, find_dependencies))
  list(type = "dim",
       value = value,
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
  template <- function(default = NULL, constant = NULL, differentiate = FALSE,
                       type = NULL) {
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

  if (is.null(args$type)) {
    args$type <- "real"
  } else {
    if (!is_scalar_character(args$type)) {
      str <- deparse1(args$differentiate)
      odin_parse_error(
        "'type' must be a scalar character, but was '{str}'",
        "E1031", src, call)
    }
    valid_types <- c("real", "integer", "logical")
    if (!(args$type %in% valid_types)) {
      odin_parse_error(
        c("Invalid value '{args$type} for argument 'type'",
          "Valid options are: {squote(valid_types)}"),
        "E1031", src, call)
    }
  }

  if (args$differentiate && isTRUE(args$constant)) {
    odin_parse_error(
      "Differentiable parameters must not be constant",
      "E1015", src, call)
  }

  if (args$differentiate && args$type != "real") {
    odin_parse_error(
      paste("Differentiable parameters must have 'type = \"real\"'",
            "not 'type = \"{args$type}\"'"),
      "E1032", src, call)
  }

  ## NOTE: this is assuming C++ types here, which is not great, but we
  ## can iron that out when thinking about the js version. It might be
  ## nicer to do the type translation at generation?
  args$type <- switch(args$type,
                      "real" = "real_type",
                      "integer" = "int",
                      "logical" = "bool")

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


parse_expr_assignment_rhs_interpolate <- function(rhs, src, call) {
  result <- match_call(rhs, function(time, value, mode) NULL)
  if (!result$success) {
    odin_parse_error(c("Invalid call to 'interpolate()'",
                       x = conditionMessage(result$error)),
                     "E1003", src, call)
  }

  tryCatch(
    mode <- match_value(result$value$mode, c("constant", "linear", "spline")),
    error = function(e) {
      odin_parse_error(
        "Invalid 'mode' argument to 'interpolate()'",
        "E1099", src, call, parent = e)
    })

  for (nm in c("time", "value")) {
    if (!rlang::is_symbol(result$value[[nm]])) {
      odin_parse_error(
        "Expected '{nm}' argument to 'interpolate()' to be a symbol",
        "E1099", src, call, parent = e)
    }
  }

  time <- as.character(result$value$time)
  value <- as.character(result$value$value)
  depends <- list(functions = character(),
                  variables = c(time, value))

  expr <- call("OdinInterpolateAlloc",
               mode = mode, time = time, value = value)

  list(type = "interpolate",
       expr = expr,
       depends = depends)
}


parse_expr_compare <- function(expr, src, call) {
  lhs <- parse_expr_compare_lhs(expr[[2]], src, call)
  rhs <- parse_expr_compare_rhs(expr[[3]], src, call)

  rhs$args <- c(lhs, rhs$args)
  rhs$depends$variables <- union(rhs$depends$variables, as.character(lhs))
  rhs$density$expr <- substitute_(
    rhs$density$expr,
    list2env(set_names(rhs$args, rhs$density$args)))
  list(rhs = rhs,
       src = src)
}


parse_expr_compare_lhs <- function(lhs, src, call) {
  if (!is.symbol(lhs)) {
    odin_parse_error(
      "The left hand side of a `~` comparison must be a symbol",
      "E1012", src, call)
  }
  lhs
}


parse_expr_compare_rhs <- function(rhs, src, call) {
  result <- monty::monty_dsl_parse_distribution(rhs, "The rhs of '~'")
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
    ignore <- "["
    if (fn_str == "sum") {
      expr <- parse_expr_usage_rewrite_reduce(expr, src, call)
    } else if (fn_str %in% monty::monty_dsl_distributions()$name) {
      expr <- parse_expr_usage_rewrite_stochastic(expr, src, call)
    } else if (fn_str %in% names(FUNCTIONS)) {
      parse_expr_check_call(expr, src, call)
      args <- lapply(expr[-1], parse_expr_usage, src, call)
      expr <- as.call(c(list(fn), args))
    } else if (!(fn_str %in% ignore)) {
      odin_parse_error(
        "Unsupported function '{fn_str}'",
        "E1027", src, call)
    }
  }
  expr
}


parse_expr_check_call <- function(expr, usage, src, call) {
  fn <- as.character(expr[[1]])
  usage <- FUNCTIONS[[fn]]
  if (is.function(usage)) {
    res <- match_call(expr, usage)
    if (!res$success) {
      err <- conditionMessage(res$error)
      odin_parse_error("Invalid call to '{fn}': {err}",
                       "E1028", src, call)
    }
  } else {
    n_args <- length(expr) - 1
    if (!is.null(names(expr))) {
      odin_parse_error(
        "Calls to '{fn}' may not have any named arguments",
        "E1029", src, call)
    }
    if (length(usage) == 1) {
      if (n_args != usage) {
        odin_parse_error(
          paste("Invalid call to '{fn}': incorrect number of arguments",
                "(expected {usage} but received {n_args})"),
          "E1030", src, call)
      }
    } else if (n_args < usage[[1]] || n_args > usage[[2]]) {
      collapse <- if (diff(usage) == 1) " or " else " to "
      usage_str <- paste(usage, collapse = collapse)
      odin_parse_error(
        paste("Invalid call to '{fn}': incorrect number of arguments",
              "(expected {usage_str} but received {n_args})"),
        "E1030", src, call)
    }
  }
  expr
}


parse_expr_usage_rewrite_stochastic <- function(expr, src, call) {
  res <- monty::monty_dsl_parse_distribution(expr)
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
                    mean = mean)
  expr[-1] <- args
  expr
}


parse_expr_usage_rewrite_reduce <- function(expr, src, call) {
  parse_expr_check_call(expr, src, call)

  fn <- as.character(expr[[1]])
  arg <- expr[[2]]
  if (rlang::is_symbol(arg)) {
    name <- as.character(arg)
    return(call("OdinReduce", fn, name, index = NULL))
  } else if (!rlang::is_call(arg, "[")) {
    odin_parse_error(
      c("Expected argument to '{fn}' to be an array",
        i = paste("The argument to '{fn}' should be name of an array (as",
                  "a symbol) to sum over all elements of the array, or",
                  "an array access (using '[]') to sum over part of",
                  "an array")),
      "E1033", src, call)
  }

  name <- as.character(arg[[2]])
  index <- as.list(arg[-(1:2)])

  ## Handle special case efficiently:
  if (all(vlapply(index, rlang::is_missing))) {
    return(call("OdinReduce", fn, name, index = NULL))
  }

  for (i in seq_along(index)) {
    v <- parse_index(name, i, index[[i]])
    deps <- v$depends
    if (!is.null(deps)) {
      if (":" %in% deps$functions) {
        odin_parse_error(
          c("Invalid use of range operator ':' within '{fn}' call",
            paste("If you use ':' as a range operator within an index,",
                  "then it must be the outermost call, for e.g,",
                  "{.code (a + 1):(b + 1)}, not {.code 1 + (a:b)}")),
          "E1034", src, call)
      }
      ## And see parse_expr_check_lhs_index for more
    }
    v$depends <- NULL
    index[[i]] <- v
  }

  call("OdinReduce", fn, name, index = index)
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


## This is something that will expand over time, there is quite a lot
## to check, really.  Things not checked:
##
## * negative numbers not allowed
## * ranges must go up
parse_expr_check_lhs_index <- function(name, dim, index, src, call) {
  ret <- parse_index(name, dim, index)

  if (is.null(ret)) {
    odin_parse_error(
      "Invalid value for array index lhs",
      "E1026", src, call)
  }

  ## We'll need to repeat most, but not all, of these checks when
  ## validating indicies used in sum/prod on the *rhs* but we will do
  ## it again as the checks are simple and the error messages need to
  ## reflect the context.
  if (any(lengths(ret$depends) > 0)) {
    if (":" %in% ret$depends$functions) {
      ## Previously in odin1 we tried to help disambiguate some calls
      ## in the error message; we might want to put that back in at
      ## some point, but it's not a big priority, most of the time
      ## this is pretty simple.
      odin_parse_error(
        c("Invalid use of range operator ':' on lhs of array assignment",
          paste("If you use ':' as a range operator on the lhs of an",
                "assignment into an array, then it must be the outermost",
                "call, for e.g, {.code (a + 1):(b + 1)}, not",
                "{.code 1 + (a:b)}")),
        "E1022", src, call)
    }
    err <- setdiff(ret$depends$functions, c("+", "-", "(", ":"))
    if (length(err) > 0) {
      odin_parse_error(
        "Invalid function{?s} used in lhs of array assignment: {squote(err)}",
        "E1023", src, call)
    }
    if ("-" %in% ret$depends$functions && uses_unary_minus(index)) {
      odin_parse_error(
        "Invalid use of unary minus in lhs of array assignment",
        "E1024", src, call)
    }
    err <- intersect(INDEX, ret$depends$variables)
    if (length(err) > 0) {
      odin_parse_error(
        paste("Invalid use of special variable{?s} in lhs of array",
              "assignment: {squote(err)}"),
        "E1025", src, call)
    }
  }

  ret$depends <- NULL
  ret
}


## Later, we could allow 'c()' in here to allow access to a series of
## values, though doing that in C++ would be a bit of a trick, but
## many things are possible now with newer C++ loops.
parse_index <- function(name_data, dim, value) {
  name_index <- INDEX[[dim]]
  if (rlang::is_missing(value)) {
    to <- call("OdinDim", name_data, dim)
    list(name = name_index, type = "range", from = 1, to = to, depends = NULL)
  } else if (rlang::is_call(value, ":")) {
    from <- value[[2]]
    to <- value[[3]]
    depends <- join_dependencies(list(find_dependencies(from),
                                      find_dependencies(to)))
    list(name = name_index, type = "range", from = from, to = to,
         depends = depends)
  } else if (is.language(value) || is.numeric(value)) {
    depends <- find_dependencies(value)
    list(name = name_index, type = "single", at = value, depends = depends)
  } else {
    NULL
  }
}
