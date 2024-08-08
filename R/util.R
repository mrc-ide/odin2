`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


squote <- function(x) {
  sprintf("'%s'", x)
}


vlapply <- function(...) {
  vapply(..., FUN.VALUE = TRUE)
}


viapply <- function(...) {
  vapply(..., FUN.VALUE = 1L)
}


vcapply <- function(...) {
  vapply(..., FUN.VALUE = "")
}


match_value <- function(x, choices, name = deparse(substitute(x)), arg = name,
                        call = NULL) {
  assert_scalar_character(x, call = call, arg = arg)
  if (!(x %in% choices)) {
    choices_str <- paste(squote(choices), collapse = ", ")
    cli::cli_abort(c("'{name}' must be one of {choices_str}",
                     i = "Instead we were given '{x}'"), call = call,
                   arg = arg)
  }
  x
}


match_call <- function(call, fn) {
  args <- names(formals(fn))
  require_unnamed <- startsWith(args, ".")

  nms_call <- names(as.list(call)[-1])
  if (any(require_unnamed)) {
    args <- gsub("^\\.", "", args)
    names(formals(fn)) <- args
    if (!is.null(nms_call)) {
      n <- sum(require_unnamed)
      if (any(nzchar(nms_call[seq_len(n)]))) {
        msg <- cli::format_inline(
          "Expected the first {n} argument{?s} to be unnamed")
        return(list(success = FALSE,
                    error = rlang::cnd("error", message = msg)))
      }
    }
  }

  res <- tryCatch(
    list(success = TRUE,
         value = rlang::call_match(call, fn, defaults = TRUE)),
    error = function(e) {
      list(success = FALSE, error = e)
    })

  if (!res$success) {
    return(res)
  }

  err_partial <- setdiff(nms_call, c(args, ""))
  if (length(err_partial)) {
    msg <- cli::format_inline(
      "Argument{?s} {squote(err_partial)} were expanded by partial matching")
    fix_partial <- vcapply(
      err_partial, function(x) args[startsWith(args, x)])
    detail <- sprintf("'%s' => '%s'", err_partial, fix_partial)
    msg <- cli::format_inline(
      "Argument{? was/s were} expanded by partial matching: {detail}")
    return(list(success = FALSE,
                error = rlang::cnd("error", message = msg)))
  }

  ## This checks that if an argument given in fn starts with a dot
  ## that its (undotted) name is not used in the call.
  if (any(require_unnamed)) {
    err <- intersect(args[require_unnamed], names(as.list(call)[-1]))
    if (length(err) > 0) {
      msg <- cli::format_inline(
        paste("Expected argument{?s} {squote(err)}",
              "to be unnamed and used first in thoe call"))
      return(list(success = FALSE,
                  error = rlang::cnd("error", message = msg)))
    }
  }

  res
}


is_scalar_logical <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
}


collector <- function(init = character(0)) {
  env <- new.env(parent = emptyenv())
  env$res <- init
  list(
    add = function(x) env$res <- c(env$res, x),
    get = function() env$res)
}


data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)
}


set_names <- function(x, nms) {
  if (length(nms) == 1 && length(x) != 1) {
    if (is.null(x)) {
      return(NULL)
    }
    nms <- rep_len(nms, length(x))
  }
  names(x) <- nms
  x
}


cli_nbsp <- function(x) {
  gsub(" ", "\u00a0", x)
}


unlist0 <- function(x) {
  unlist(x, FALSE, FALSE)
}


## This is definitely possible with rlang, but I am not sure how.
substitute_ <- function(expr, env) {
  eval(substitute(substitute(y, env), list(y = expr)))
}
