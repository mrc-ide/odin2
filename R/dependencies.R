find_dependencies <- function(expr) {
  functions <- collector()
  variables <- collector()
  descend <- function(e) {
    if (is.recursive(e)) {
      if (rlang::is_call(e) && is.symbol(e[[1]])) {
        functions$add(deparse(e[[1L]]))
      }
      is_dim_read <- rlang::is_call(e, c("length", "dim")) &&
        length(e) == 2 && is.symbol(e[[2]])
      if (is_dim_read) {
        variables$add(odin_dim_name(deparse(e[[2]])))
      } else {
        for (el in as.list(e[-1])) {
          if (!missing(el)) {
            descend(el)
          }
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


join_dependencies <- function(x) {
  stopifnot(is.list(x))
  x <- x[!vlapply(x, is.null)]
  ok <- vlapply(x, function(el) {
    identical(names(el), c("functions", "variables"))
  })
  stopifnot(all(ok))
  if (length(x) == 0L) {
    list(functions = character(0), variables = character(0))
  } else if (length(x) == 1L) {
    x[[1L]]
  } else {
    list(functions = unique(unlist(lapply(x, "[[", "functions"))),
         variables = unique(unlist(lapply(x, "[[", "variables"))))
  }
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


collapse_dependencies <- function(deps) {
  nms <- names(deps)
  if (!anyDuplicated(nms)) {
    return(deps)
  }
  i <- duplicated(nms)
  dups <- unique(nms[i])
  deps[dups] <- lapply(dups, function(nm) unique(unlist0(deps[nms == nm])))
  deps[!i]
}
