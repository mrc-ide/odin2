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

  list(special = special,
       lhs = lhs,
       rhs = rhs,
       src = src)
}


parse_expr_assignment_lhs <- function(lhs, src, call) {
  array <- NULL
  special <- NULL
  name <- NULL

  if (rlang::is_call(lhs, SPECIAL_LHS)) {
    special <- deparse1(lhs[[1]])
    if (length(lhs) != 2 || !is.null(names(lhs))) {
      odin_parse_error(
        c("Invalid special function call",
          i = "Expected a single unnamed argument to '{special}()'"),
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
  ## operator but I'm not totlaly sure that's the best place to do so.
  depends <- find_dependencies(rhs)
  list(type = "expression",
       expr = rhs,
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

  ## Quickly rewrite the expression, at least for now:
  rhs$expr <- as.call(c(list(rhs$expr[[1]], lhs),
                        as.list(rhs$expr[-1])))
  rhs$depends$variables <- union(rhs$depends$variables,
                                 as.character(lhs))
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
  list(type = "compare",
       distribution = result$value$cpp$density,
       args = result$value$args,
       depends = depends)
}


parse_expr_print <- function(expr, src, call) {
  odin_parse_error(
    "'print()' is not implemented yet", "E0001", src, call)
}
