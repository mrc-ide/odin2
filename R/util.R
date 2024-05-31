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
