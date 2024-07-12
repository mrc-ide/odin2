find_dependencies <- function(expr) {
  functions <- collector()
  variables <- collector()
  descend <- function(e) {
    if (is.recursive(e)) {
      nm <- deparse(e[[1L]])
      ## If we hit dim/length here we should not take a dependency
      ## most of the time though.
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
