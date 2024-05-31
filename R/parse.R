odin_parse <- function(expr, input = NULL) {
  call <- environment()
  dat <- parse_prepare(rlang::enquo(expr), input, call)
  exprs <- lapply(dat$exprs, function(x) parse_expr(x$value, x, call = call))
  system <- parse_system(exprs, call)
  ret <- parse_depends(system, call)
  ## This changes immensely once we have arrays as we need to work
  ## with a more flexible packing structure.  For now we just cheat
  ## and assume variables are packed in order as they are all scalars.
  ret
}


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


parse_system <- function(exprs, call) {
  special <- vcapply(exprs, function(x) x$lhs$special %||% "")
  is_lhs_update <- special == "update"
  is_lhs_deriv <- special == "deriv"
  is_lhs_output <- special == "output"
  is_lhs_initial <- special == "initial"
  is_lhs_compare <- special == "compare"
  is_equation <- special == ""
  name_data <- vcapply(exprs, function(x) x$lhs$name)

  ## We take initial as the set of variables:
  variables <- name_data[is_lhs_initial]
  if (length(variables) == 0) {
    odin_parse_error("Did not find any call to 'initial()'", NULL, call)
  }

  src <- lapply(exprs, "[[", "src")

  ## Check what sort of system we even have:
  is_continuous <- any(is_lhs_deriv)
  is_discrete <- any(is_lhs_update)
  if (is_continuous && is_discrete) {
    odin_parse_error(
      "Can't support both 'update()' and 'deriv()' within a single model yet",
      src[is_lhs_deriv | is_lhs_update], call)
  }

  if (any(is_lhs_output)) {
    odin_parse_error(
      "Can't support both 'output()' yet",
      src[is_lhs_output], call)
  }

  target <- if (is_continuous) "deriv" else "update"
  is_lhs_target <- special == target

  variables_target <- name_data[is_lhs_target]
  if (!setequal(variables, variables_target)) {
    common <- intersect(variables, variables_target)
    err <- (is_lhs_target | is_lhs_initial) & !(variables %in% common)
    odin_parse_error(
      "Different equations for 'initial()' and '{target}()'",
      src[err], call)
  }

  ## Then we break equations up:
  exprs <- list(equations = exprs[is_equation],
                update = exprs[is_lhs_update],
                deriv = exprs[is_lhs_deriv],
                output = exprs[is_lhs_output],
                initial = exprs[is_lhs_initial],
                compare = exprs[is_lhs_compare])
  list(time = if (is_continuous) "continuous" else "discrete",
       variables = variables,
       exprs = exprs)
}


parse_check_lhs_name <- function(lhs, src, call) {
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


parse_expr_compare <- function(expr, src, call) {
  .NotYetImplemented()
}


parse_expr_print <- function(expr, src, call) {
  .NotYetImplemented()
}


odin_parse_error <- function(msg, src, expr, .envir = parent.frame()) {
  cli::cli_abort(msg,
                 class = "odin_parse_error",
                 src = src,
                 expr = expr,
                 call = call,
                 .envir = .envir)
}
