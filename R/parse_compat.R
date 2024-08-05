parse_compat <- function(exprs, call) {
  exprs <- lapply(exprs, parse_compat_fix_user, call)
  exprs
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
    for (arg in c("integer", "min", "max")) {
      if (!rlang::is_missing(res$value[[arg]])) {
        odin_parse_error(
          c("Can't yet translate 'user()' calls that use the '{arg}' argument",
            i = paste("We don't support this argument in parameter() yet,",
                      "but once we do we will support translation")),
          "E0001", expr, call = call)
      }
    }

    args <- list(as.name("parameter"))
    if (!rlang::is_missing(res$value$default)) {
      args <- c(args, list(res$value$default))
    }
    expr$value[[3]] <- as.call(args)
    expr$compat <- list(type = "user", original = expr$value)
  }
  expr
}
