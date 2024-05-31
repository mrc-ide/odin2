find_dependencies <- function(expr) {
  functions <- collector()
  variables <- collector()
  descend <- function(e) {
    if (is.recursive(e)) {
      nm <- deparse(e[[1L]])
      if (nm %in% c("dim", "length")) {
        stop("Think about this")
      }
      functions$add(nm)
      for (el in as.list(e[-1])) {
        if (!missing(el)) {
          descend(el)
        }
      }
    } else {
      if (is.symbol(e)) {
        variables$add(deparse(e))
      }
    }
  }
  descend(expr)
  list(functions = unique(functions$get()),
       variables = unique(variables$get()))

}


collector <- function(init = character(0)) {
  env <- new.env(parent = emptyenv())
  env$res <- init
  list(
    add = function(x) env$res <- c(env$res, x),
    get = function() env$res)
}
