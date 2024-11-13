expr_to_sum_of_parts <- function(expr) {
  if (rlang::is_call(expr, c("-", "+"), 2)) {
    if (rlang::is_call(expr, "-")) {
      parts <- lapply(expr[-1], expr_to_sum_of_parts)
      uminus <- monty::monty_differentiation()$maths$uminus
      parts[[2]] <- lapply(parts[[2]], uminus)
      unlist(parts, FALSE)
    } else {
      unlist(lapply(expr[-1], expr_to_sum_of_parts), FALSE)
    }
  } else {
    list(expr)
  }
}


expr_factorise <- function(x) {
  f <- function(el) {
    if (is.numeric(el)) {
      list(el, 1, "")
    } else if (rlang::is_call(el, "-", 1)) {
      ret <- f(el[[2]])
      ret[[1]] <- -1 * ret[[1]]
      ret
    } else {
      list(1, el, rlang::hash(el))
    }
  }
  maths <- monty::monty_differentiation()$maths
  parts <- lapply(expr_to_sum_of_parts(x), f)
  id <- vcapply(parts, "[[", 3)
  ret <- lapply(unname(split(parts, id)), function(el) {
    n <- sum(vnapply(el, "[[", 1))
    maths$times(n, el[[1]][[2]])
  })
  maths$plus_fold(ret)
}
