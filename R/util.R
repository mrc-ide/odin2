`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE, check.names = FALSE)
}


vlapply <- function(...) {
  vapply(..., FUN.VALUE = TRUE)
}


vcapply <- function(...) {
  vapply(..., FUN.VALUE = "")
}


set_names <- function(x, nms) {
  names(x) <- nms
  x
}


squote <- function(x) {
  sprintf("'%s'", x)
}


collector <- function(init = character(0)) {
  env <- new.env(parent = emptyenv())
  env$res <- init
  list(
    add = function(x) env$res <- c(env$res, x),
    get = function() env$res)
}


match_call <- function(call, fn) {
  ## We'll probably expand on the error case here to return something
  ## much nicer?

  ## TODO: it would be great to totally prevent partial matching here.
  ## The warning emitted by R is not easily caught (no special class
  ## for example) and neither match.call nor the rlang wrapper provide
  ## a hook here to really pick this up.  We can look for expanded
  ## names in the results, though that's not super obvious either
  ## since we're also filling them in and reordering.
  tryCatch(
    list(success = TRUE,
         value = rlang::call_match(call, fn, defaults = TRUE)),
    error = function(e) {
      list(success = FALSE,
           error = e)
    })
}


is_scalar_logical <- function(x) {
  is.logical(x) && length(x) == 1 && !is.na(x)
}
