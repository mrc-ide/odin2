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
      src, call)
  }
}


parse_expr_assignment <- function(expr, src, call) {
  lhs <- parse_expr_assignment_lhs(expr[[2]], src, call)
  rhs <- parse_expr_assignment_rhs(expr[[3]], src, call)

  special <- lhs$special
  lhs$special <- NULL
  if (is.null(special) && rhs$type == "data") {
    special <- "data"
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
    if (length(lhs) != 2 || !is.null(names(lhs))) {
      odin_parse_error("Invalid special function call", src, call)
    }
    special <- deparse1(lhs[[1]])
    lhs <- lhs[[2]]
  }
  is_array <- rlang::is_call(lhs, "[")

  if (is_array) {
    name <- parse_expr_check_lhs_name(lhs[[1]], src, call)
    array <- as.list(lhs[-(1:2)])
  } else {
    name <- parse_expr_check_lhs_name(lhs, src, call)
  }

  lhs <- list(
    name = name,
    array = array,
    special = special)
}


parse_expr_assignment_rhs <- function(rhs, src, call) {
  if (rlang::is_call(rhs, "delay")) {
    stop("Implement delays")
  } else if (rlang::is_call(rhs, "parameter")) {
    stop("Implement parameters")
  } else if (rlang::is_call(rhs, "data")) {
    parse_expr_assignment_rhs_data(rhs, src, call)
  } else if (rlang::is_call(rhs, "interpolate")) {
    stop("Implement interpolation")
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
    odin_parse_error("Expected a symbol on the lhs", src, call)
  }
  name <- deparse1(lhs)
  name
}


parse_expr_assignment_rhs_data <- function(rhs, src, call) {
  if (length(rhs) != 1) {
    odin_parse_error("Calls to 'data()' must have no arguments",
                     src, call)
  }
  list(type = "data")
}


parse_expr_compare <- function(expr, src, call) {
  .NotYetImplemented()
}


parse_expr_print <- function(expr, src, call) {
  .NotYetImplemented()
}
