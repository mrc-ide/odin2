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


topological_order <- function(deps) {
  if (all(lengths(deps) == 0)) {
    return(list(success = TRUE, value = seq_along(deps)))
  }

  m <- matrix(FALSE, length(deps), length(deps))
  for (i in seq_along(deps)) {
    m[, i] <- unname(names(deps) %in% deps[[i]])
  }

  pending <- rep(TRUE, length(deps))
  ret <- integer(0)
  while (any(pending)) {
    i <- which(pending)[colSums(m[, pending, drop = FALSE]) == 0]
    if (length(i) > 0L) {
      ret <- c(ret, i)
      pending[i] <- FALSE
      m[i, ] <- FALSE
    } else {
      return(list(success = FALSE, error = which(pending)))
    }
  }

  list(success = TRUE, value = ret)
}
