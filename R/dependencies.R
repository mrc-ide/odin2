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


topological_order <- function(deps) {
  if (all(lengths(deps) == 0)) {
    return(seq_along(deps))
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
      f <- function(i) {
        sprintf("\t%s: depends on %s", names(ret)[[i]],
                paste(err[m[pending, i]], collapse = ", "))
      }
      err <- names(ret)[pending]
      detail <- paste(vcapply(which(pending), f), collapse = "\n")
      stop(sprintf("A cyclic dependency detected for %s:\n%s",
                   paste(names(ret)[pending], collapse = ", "),
                   detail), call. = FALSE)
    }
  }

  ret
}
