parse_expr <- function(expr, src, call) {
  if (rlang::is_call(expr, c("<-", "="))) {
    parse_expr_assignment(expr, src, call)
  } else if (rlang::is_call(expr, "~")) {
    parse_expr_compare(expr, src, call)
  } else if (rlang::is_call(expr, "print")) {
    parse_expr_print(expr, src, call)
  } else if (rlang::is_call(expr, "browser")) {
    parse_expr_browser(expr, src, call)
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
    lhs$name <- odin_dim_name(lhs$name)
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
    if (!is.null(rhs$args$rank)) {
      odin_parse_error(
        paste("Invalid use of 'rank' argument in 'parameter()' call not",
              "assigning to dimension"),
        "E1055", src, call)
    }
    special <- "parameter"
  } else if (rhs$type == "interpolate") {
    if (!is.null(special) || !is.null(lhs$array)) {
      odin_parse_error(
        "Calls to 'interpolate()' must be assigned to a symbol",
        "E1037", src, call)
    }
    lhs$name_data <- lhs$name
    lhs$name <- paste0("interpolate_", lhs$name)
  } else if (rhs$type == "delay") {
    if (!is.null(special) || !is.null(lhs$array)) {
      odin_parse_error(
        "Calls to 'delay()' must be assigned to a symbol",
        "E1070", src, call)
    }
    special <- "delay"
  } else {
    if (rlang::is_call(rhs$expr, "as.integer")) {
      lhs$storage_type <- "int"
    } else if (rlang::is_call(rhs$expr, "as.logical")) {
      lhs$storage_type <- "bool"
    }
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

  rhs$depends$variables <- parse_expr_check_index_usage(
    rhs$depends$variables, lhs$array, src, call)

  if (lhs$name %in% rhs$depends$variables) {
    allow_self_reference <-
      !is.null(lhs$array) ||
      ((special %||% "") %in% c("update", "deriv"))
    if (!allow_self_reference) {
      rhs$depends$variables <- setdiff(rhs$depends$variables, lhs$name)
      odin_parse_error(
        c("Equation '{lhs$name}' cannot reference itself",
          i = paste("Your equation references itself in its right-hand side,",
                    "which is only allowed for array equations or",
                    "update() and deriv() expressions")),
        "E1038", src, call)
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
    dim = function(...) NULL,
    config = function(name) NULL)

  args <- NULL
  if (rlang::is_call(lhs, names(special_def))) {
    special <- deparse1(lhs[[1]])
    m <- match_call(lhs, special_def[[special]])
    if (!m$success) {
      odin_parse_error(c("Invalid call to special function '{special}'",
                         x = conditionMessage(m$error)),
        "E1003", src, call)
    }
    if (rlang::is_missing(m$value$name)) {
      odin_parse_error(
        c("Invalid call to special function '{special}'",
          i = paste("Missing target for '{special}()', typically the first",
                    "(unnamed) argument")),
        "E1003", src, call)
    }

    if (special == "dim") {
      if (length(lhs) < 2) {
        odin_parse_error(
          "Invalid call to 'dim()' on lhs; no variables given",
          "E1003", src, call)
      }
      lhs <- vcapply(lhs[-1], function(x) {
        if (!is.symbol(x)) {
          odin_parse_error(
            "Invalid call to 'dim()' on lhs; '{deparse1(x)}' is not a symbol",
            "E1005", src, call)
        }
        parse_expr_check_lhs_name(x, special, is_array, src, call)
      })

      return(list(
        name = lhs[[1]], # may be more than one if dims are aliased
        names = lhs,
        special = special))
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
    name <- parse_expr_check_lhs_name(lhs[[2]], special, is_array, src, call)
    array <- Map(parse_expr_check_lhs_index,
                 seq_len(length(lhs) - 2),
                 lhs[-(1:2)],
                 MoreArgs = list(name = name, src = src, call = call))
    depends <- join_dependencies(
      lapply(array, function(x)
        join_dependencies(lapply(x[c("at", "from", "to")], find_dependencies))))
  } else {
    name <- parse_expr_check_lhs_name(lhs, special, is_array, src, call)
    array <- NULL
    depends <- NULL
  }

  lhs <- list(
    name = name,
    special = special,
    array = array,
    depends = depends)

  if (!is.null(args)) {
    lhs$args <- args
  }

  lhs
}


parse_expr_assignment_rhs <- function(rhs, src, call) {
  if (rlang::is_call(rhs, "delay")) {
    parse_expr_assignment_rhs_delay(rhs, src, call)
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

  special_rhs <- c("parameter", "interpolate", "data", "delay")
  err <- intersect(special_rhs, depends$functions)
  if (length(err) > 0) {
    odin_parse_error(
      "'{err[[1]]}()' must be the only call on the rhs if used",
      "E1041", src, call)
  }

  special_lhs <- c("initial", "update", "deriv", "output", "config")
  err <- intersect(special_lhs, c(depends$functions, depends$variables))
  if (length(err) > 0) {
    odin_parse_error(
      paste("Special function '{err[[1]]}' is not allowed on the rhs",
            "of an expression"),
      "E1042", src, call)
  }

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
  throw_no_stochastic <- function() {
    odin_parse_error(
      "Array extent cannot be stochastic",
      "E1039", src, call)
  }

  throw_cant_determine_extent <- function() {
    odin_parse_error(
      "Array extent cannot be determined by time",
      "E1040", src, call)
  }

  throw_invalid_rhs_dim <- function(err) {
    odin_parse_error(
      "Invalid function{?s} used on rhs of 'dim()': {squote(err)}",
      "E1043", src, call)
  }

  throw_rank_needed <- function() {
    odin_parse_error(
      "When using 'dim() <- parameter(...)', a 'rank' argument is required",
      "E1056", src, call)
  }

  throw_bad_rank_arg <- function() {
    odin_parse_error(
      "'rank' must be a scalar size, if given",
      "E1057", src, call)
  }

  throw_bad_dim_arg <- function() {
    odin_parse_error(
      "When using 'dim()' on the right-hand-side, it takes only an array name",
      "E1066", src, call)
  }

  is_user_sized <- rlang::is_call(rhs, "parameter")
  if (is_user_sized) {
    if (is.null(rhs$rank)) {
      throw_rank_needed()
    }
    if (!is_scalar_size(rhs$rank)) {
      throw_bad_rank_arg()
    }
    value <- vector("list", rhs$rank)
  } else if (rlang::is_call(rhs, "c")) {
    value <- as.list(rhs[-1])
  } else {
    value <- list(rhs)
  }
  depends <- join_dependencies(lapply(value, find_dependencies))
  is_stochastic <- any(
    depends$functions %in% monty::monty_dsl_distributions()$name)
  if (is_stochastic) {
    throw_no_stochastic()
  }
  if ("time" %in% depends$variables) {
    throw_cant_determine_extent()
  }

  if (rlang::is_call(rhs, "dim")) {
    if (!is.symbol(rhs[[2]])) {
      throw_bad_dim_arg()
    }
    return(list(type = "dim",
                is_user_sized = FALSE,
                value = rhs,
                depends = list(
                  functions = character(0),
                  variables = depends$variables)))
  }
  allowed <- c("+", "-", "(", "length", "nrow", "ncol")
  err <- setdiff(depends$functions, allowed)
  if (length(err) > 0) {
    throw_invalid_rhs_dim(err)
  }
  list(type = "dim",
       is_user_sized = is_user_sized,
       value = value,
       depends = depends)
}


parse_expr_check_lhs_name <- function(lhs, special, is_array, src, call) {
  is_compare <- identical(special, "~")

  if (!rlang::is_symbol(lhs)) {
    ## We will error, the only question is how.
    if (is_array) {
      if (is_compare) {
        context <- "on the lhs of a `~` array comparison"
      } else if (is.null(special)) {
        context <- "on the lhs of array assignment"
      } else {
        context <- sprintf("within '%s()' on the lhs of array assignment",
                           special)
      }
    } else {
      if (is_compare) {
        context <- "on the lhs of a `~` comparison"
      } else if (is.null(special)) {
        context <- "on the lhs of assignment"
      } else  {
        context <- sprintf("within '%s()' on the lhs of assignment", special)
      }
    }
    lhs_str <- deparse1(lhs)

    if (!rlang::is_call(lhs)) {
      odin_parse_error("Invalid target '{lhs_str}' {context}",
                       "E1005", src, call)
    }

    fn_str <- deparse1(lhs[[1]])
    if (is_compare || is.null(special)) {
      if (is_array && rlang::is_call(lhs, SPECIAL_LHS) && length(lhs) >= 2) {
        target <- deparse1(lhs[[2]])
        hint <- paste("Did you mean '{fn_str}({target}[...])' rather than",
                      "'{fn_str}({target})[...]'")
        odin_parse_error(
          c("Invalid array access outside of special function '{fn_str}()'",
            i = hint),
          "E1005", src, call)
      }
      fn_near <- near_match(fn_str, SPECIAL_LHS)
      if (!is_compare && length(fn_near) == 1) {
        hint <- c(i = "Did you mean '{fn_near}()'?")
      } else {
        hint <- NULL
      }
      odin_parse_error(
        c("Invalid special function '{fn_str}()' {context}",
          hint),
        "E1005", src, call)
    } else {
      if (rlang::is_call(lhs, SPECIAL_LHS)) {
        odin_parse_error(
          "Invalid nested special lhs function '{fn_str}' within '{special}'",
          "E1005", src, call)
      } else {
        odin_parse_error("Invalid target '{lhs_str}' {context}",
                         "E1005", src, call)
      }
    }
  }
  name <- deparse1(lhs)

  if (name %in% RESERVED) {
    err_in <- c(if (name %in% RESERVED_ODIN) "odin",
                if (name %in% RESERVED_CPP) "C++",
                if (name %in% RESERVED_JS) "JavaScript")
    odin_parse_error(
      c("Can't assign to reserved name '{name}'",
        i = "'{name}' is a reserved word in {err_in}"),
      "E1004", src, call)
  }
  if (grepl(RESERVED_ODIN_PREFIX_RE, name)) {
    prefix <- sub(RESERVED_ODIN_PREFIX_RE, "\\1", name)
    odin_parse_error(
      "Invalid name '{name}' starts with reserved prefix '{prefix}'",
      "E1047", src, call)
  }
  if (name == "pi") {
    odin_parse_error(
      "Do not use `pi` on the left-hand-side of an expression",
      "E1061", src, call)
  }

  name
}


parse_expr_assignment_rhs_parameter <- function(rhs, src, call) {
  template <- function(default = NULL, constant = NULL, differentiate = FALSE,
                       type = NULL, rank = NULL, min = NULL, max = NULL) {
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
    args$type <- if (args$differentiate) "real" else NA_character_
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

  if (args$differentiate) {
    if (isTRUE(args$constant)) {
      odin_parse_error(
        "Differentiable parameters must not be constant",
        "E1015", src, call)
    }
    args$constant <- FALSE
  }

  if (args$differentiate && args$type != "real") {
    odin_parse_error(
      paste("Differentiable parameters must have 'type = \"real\"'",
            "not 'type = \"{args$type}\"'"),
      "E1032", src, call)
  }

  for (nm in c("min", "max")) {
    if (!is.null(args[[nm]])) {
      if (identical(args$type, "logical")) {
        odin_parse_error(
          "'{nm}' cannot be used with 'type = \"logical\"'",
          "E1063", src, call)
      }
      if (!is_scalar_numeric(args[[nm]])) {
        odin_parse_error(
          "'{nm}' must be a number",
          "E1064", src, call)
      }
    }
  }

  if (!is.null(args$min) && !is.null(args$max) && args$min >= args$max) {
    odin_parse_error("'min' must be smaller than 'max'",
                     "E1065", src, call)
  }

  ## NOTE: this is assuming C++ types here, which is not great, but we
  ## can iron that out when thinking about the js version. It might be
  ## nicer to do the type translation at generation? See also above
  ## where we use int/bool too.
  if (!is.na(args$type)) {
    args$type <- switch(args$type,
                        "real" = "real_type",
                        "integer" = "int",
                        "logical" = "bool")
  }

  list(type = "parameter",
       args = args,
       depends = list(functions = character(),
                      variables = character()))
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
                     "E1035", src, call)
  }
  is_missing <- vlapply(result$value, rlang::is_missing)
  if (any(is_missing)) {
    msg <- names(result$value)[is_missing]
    odin_parse_error(c("Invalid call to 'interpolate()'",
                       x = "Missing argument{?s} {squote(msg)}"),
                     "E1035", src, call)
  }

  for (nm in c("time", "value")) {
    if (!rlang::is_symbol(result$value[[nm]])) {
      odin_parse_error(
        "Expected '{nm}' argument to 'interpolate()' to be a symbol",
        "E1035", src, call)
    }
  }
  tryCatch(
    mode <- match_value(result$value$mode, c("constant", "linear", "spline")),
    error = function(e) {
      odin_parse_error(
        "Invalid 'mode' argument to 'interpolate()'",
        "E1036", src, call, parent = e)
    })


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


parse_expr_assignment_rhs_delay <- function(rhs, src, call) {
  result <- match_call(rhs, function(what, by) NULL)
  if (!result$success) {
    odin_parse_error(c("Invalid call to 'delay()'",
                       x = conditionMessage(result$error)),
                     "E1067", src, call)
  }
  is_missing <- vlapply(result$value, rlang::is_missing)
  if (any(is_missing)) {
    msg <- names(result$value)[is_missing]
    odin_parse_error(c("Invalid call to 'delay()'",
                       x = "Missing argument{?s} {squote(msg)}"),
                     "E1067", src, call)
  }

  what <- result$value$what
  if (!rlang::is_symbol(what)) {
    odin_parse_error(
      "Expected 'what' argument to 'delay()' to be a symbol",
      "E1068", src, call)
  }
  what <- as.character(what)
  by <- result$value$by
  if (!(rlang::is_symbol(by) || is.numeric(by))) {
    odin_parse_error(
      "Expected 'by' argument to 'delay()' to be a number or symbol",
      "E1069", src, call)
  }

  depends <- find_dependencies(by)

  list(type = "delay",
       expr = call("OdinDelay", what = what, by = by),
       depends = depends)
}


parse_expr_compare <- function(expr, src, call) {
  lhs <- parse_expr_compare_lhs(expr[[2]], src, call)
  rhs <- parse_expr_compare_rhs(expr[[3]], src, call)
  rhs$args <- c(lhs$expr, rhs$args)
  rhs$depends$variables <- unique(c(rhs$depends$variables,
                                    lhs$depends$variables,
                                    lhs$name))
  rhs$depends$variables <- parse_expr_check_index_usage(
    rhs$depends$variables, lhs$array, src, call)
  rhs$density$expr <- substitute_(
    rhs$density$expr,
    list2env(set_names(rhs$args, rhs$density$args)))
  list(FIXME = list(name = lhs$name),
       rhs = rhs,
       src = src,
       array = lhs$array)
}


parse_expr_compare_lhs <- function(lhs, src, call) {
  is_array <- rlang::is_call(lhs, "[")

  ## Duplicates a little code within parse_expr_assignment_lhs(); we
  ## might want to split the end of that off?
  if (is_array) {
    name <- parse_expr_check_lhs_name(lhs[[2]], "~", is_array, src, call)
    array <- Map(parse_expr_check_lhs_index,
                 seq_len(length(lhs) - 2),
                 lhs[-(1:2)],
                 MoreArgs = list(name = name, src = src, call = call))
    depends <- join_dependencies(
      lapply(array, function(x)
        join_dependencies(lapply(x[c("at", "from", "to")], find_dependencies))))
    expr <- lhs
    expr[-(1:2)] <- lapply(INDEX[seq_len(length(expr) - 2)], as.symbol)
  } else {
    name <- parse_expr_check_lhs_name(lhs, "~", is_array, src, call)
    expr <- lhs
    array <- NULL
    depends <- NULL
  }

  list(name = name,
       expr = expr,
       array = array,
       depends = depends)
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
  args <- lapply(result$value$args, parse_expr_usage, src, call)

  list(type = "compare",
       density = density,
       args = args,
       depends = depends)
}


parse_expr_print <- function(expr, src, call) {
  ## TODO: we might add a 'where' argument in future which controls
  ## which of the generated functions we call the print from.  At
  ## present it will be in deriv/update only, matching odin1, or we'll
  ## give it a phase like anything else?
  m <- match_call(expr, function(string, when = NULL) NULL)
  if (!m$success) {
    odin_parse_error(
      "Failed to parse 'print()' statement",
      parent = m$error,
      "E1052", src, call)
  }

  string <- m$value$string
  inputs <- parse_print_string(string, src, call)
  depends <- join_dependencies(
    lapply(inputs, function(x) find_dependencies(x$expr)))

  list(special = "print",
       rhs = list(type = "print"), # makes checking easier elsewhere
       string = string,
       inputs = inputs,
       depends = depends,
       when = m$value$when,
       src = src)
}


parse_print_string <- function(string, src, call) {
  if (!rlang::is_string(string)) {
    odin_parse_error(
      "Expected the first argument to 'print()' to be a string",
      "E1053", src, call)
  }

  parts <- tryCatch(
    as.list(glue_find_variables(string)),
    error = function(e) {
      odin_parse_error("Failed to parse '{string}' with 'glue'",
                       "E1053", src, call, parent = e)
    })

  if (length(parts) == 0) {
    ## This would make no sense, and is likely an error from the user.
    odin_parse_error(
      c("Invalid 'print()' expression does not reference any values",
        i = paste("You provided the string '{string}', which does not",
                  "contain any expressions within curly braces ({{...}})")),
      "E1053", src, call)
  }

  lapply(parts, function(p) {
    tryCatch(
      parse_print_element(p), error = function(e) {
        odin_parse_error(
          "Failed to parse print string '{string}': '{p}' is not valid",
          "E1054", src, call, parent = e)
      })
  })
}


parse_print_element <- function(str) {
  re <- "(.+)\\s*;\\s*(.+)"
  has_format <- grepl(re, str)
  if (has_format) {
    format <- sub(re, "\\2", str)
    ## Try applying the format in; we'll error here and be caught
    ## above if this is not interpretable.
    sprintf(paste0("%", format), 1)
    value <- sub(re, "\\1", str)
  } else {
    format <- NULL
    value <- str
  }

  expr <- parse(text = value)[[1]]

  list(expr = expr, format = format)
}


parse_expr_browser <- function(expr, src, call) {
  fn <- function(phase, when = NULL) NULL
  m <- match_call(expr, fn)
  if (!m$success) {
    odin_parse_error(
      "Failed to parse 'browser()' call",
      "E1059", src, call, parent = m$error)
  }

  phase <- m$value$phase
  when <- m$value$when

  tryCatch(
    match_value(phase, PHASES_BROWSER),
    error = function(e) {
      odin_parse_error(
        "Invalid value for 'phase' argument to 'browser()'",
        "E1060", src, call, parent = e)
    })

  depends <- find_dependencies(m$value$when)
  list(type = "browser",
       special = "browser",
       rhs = list(type = "browser"), # makes checking easier elsewhere
       phase = phase,
       when = when,
       depends = depends,
       src = src)
}


parse_expr_usage <- function(expr, src, call) {
  if (is.recursive(expr)) {
    fn <- expr[[1]]
    if (!is.symbol(fn)) {
      odin_parse_error(
        c("Unsupported expression used as function '{deparse1(fn)}()'",
          i = "Functions must be symbols, not numbers or other expressions"),
        "E1044", src, call)
    }
    fn_str <- as.character(fn)
    ignore <- "["
    is_reduction <- fn_str %in% c("sum", "prod") ||
      (fn_str %in% c("min", "max") && length(expr) == 2)
    if (is_reduction) {
      expr <- parse_expr_usage_rewrite_reduce(expr, src, call)
    } else if (fn_str %in% monty::monty_dsl_distributions()$name) {
      expr <- parse_expr_usage_rewrite_stochastic(expr, src, call)
    } else if (fn_str %in% names(FUNCTIONS)) {
      parse_expr_check_call(expr, src, call)
      args <- lapply(expr[-1], parse_expr_usage, src, call)
      expr <- as.call(c(list(fn), args))
    } else if (fn_str %in% c("function", "while", "repeat", "for")) {
      ## Slightly better message than below, as these are not really
      ## functions
      odin_parse_error(
        "Can't use '{fn_str}' within odin code",
        "E1045", src, call)
    } else if (!(fn_str %in% ignore)) {
      odin_parse_error(
        "Unsupported function '{fn_str}'",
        "E1027", src, call)
    }
  } else {
    if (rlang::is_na(expr)) {
      odin_parse_error(
        "Cannot use '{deparse(expr)}' within expressions",
        "E1062", src, call)
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
        if (fn == "if" && n_args == 2) {
          odin_parse_error(
            "All 'if' statements must have an 'else' clause",
            "E1046", src, call)

        } else {
          odin_parse_error(
            paste("Invalid call to '{fn}': incorrect number of arguments",
                  "(expected {usage} but received {n_args})"),
            "E1030", src, call)
        }
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
              names(formals(res$value$density))[-1]))

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
    return(call("OdinReduce", fn = fn, what = name, index = NULL, expr = expr))
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
    return(call("OdinReduce", fn = fn, what = name, index = NULL, expr = expr))
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

  call("OdinReduce", fn = fn, what = name, index = index, expr = expr)
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
    allowed <- c("+", "-", "(", ":", "length", "nrow", "ncol")
    err <- setdiff(ret$depends$functions, allowed)
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


parse_expr_check_index_usage <- function(variables, array, src, call) {
  index_used <- intersect(INDEX, variables)
  if (length(index_used) > 0) {
    n <- length(array)
    err <- if (n == 0) index_used else intersect(index_used, INDEX[-seq_len(n)])
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
    variables <- setdiff(variables, INDEX)
  }
  variables
}
