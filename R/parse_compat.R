parse_compat <- function(exprs, action, call) {
  ## Things we can translate:
  ##
  ## user() -> parameter()
  ## rbinom() -> Binomial() [etc]
  ## dt -> drop
  ## step -> time
  exprs <- lapply(exprs, parse_compat_fix_user, call)
  parse_compat_report(exprs, action, call)
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

    original <- expr$value
    args <- list(as.name("parameter"))
    if (!rlang::is_missing(res$value$default)) {
      args <- c(args, list(res$value$default))
    }
    expr$value[[3]] <- as.call(args)
    expr$compat <- list(type = "user", original = original)
  }
  expr
}


parse_compat_report <- function(exprs, action, call) {
  i <- !vlapply(exprs, function(x) is.null(x$compat))
  if (action != "silent" && any(i)) {
    description <- c(
      user = "Replace calls to 'user()' with 'parameter()'")
    type <- vcapply(exprs[i], function(x) x$compat$type)

    detail <- NULL
    for (t in intersect(names(description), type)) {
      j <- i[type == t]
      ## Getting line numbers here is really hard, so let's just not
      ## try for now and do this on deparsed expressions.
      updated <- vcapply(exprs[j], function(x) deparse1(x$value))
      original <- vcapply(exprs[j], function(x) deparse1(x$compat$original))
      context_t <- set_names(
        c(rbind(updated, original, deparse.level = 0)),
        rep(c("x", "v"), length(updated)))
      detail <- c(detail, description[[t]], cli_nbsp(context_t))
    }

    header <- "Found {sum(i)} compatibility issue{?s}"

    if (action == "error") {
      odin_parse_error(c(header, detail), "E1017", exprs[i], call)
    } else {
      cli::cli_warn(c(header, detail), call = call)
    }
  }
}
