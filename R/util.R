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
  ## much nicer.
  tryCatch(
    list(success = TRUE,
         args = as.list(rlang::call_match(call, fn, defaults = TRUE))[-1]),
    error = function(e) {
      list(success = FALSE,
           error = e)
    })
}
