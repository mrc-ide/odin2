parse_adjoint <- function(dat) {
  ## We need to make sure that we can work with this system; that
  ## requires that we have at least one bit of comparison to data and
  ## at least one differentiable parameter).
  variables <- dat$variables
  data <- dat$data$name
  parameters <- dat$parameters$name[dat$parameters$differentiate]

  if (length(parameters) == 0) {
    return(dat)
  }

  ## TODO: validate that we have data/compare because otherwise we
  ## have nothing to differentiate!

  deps <- lapply(dat$equations, function(eq) eq$rhs$depends$variables_recursive)

  dat$adjoint <- list(
    update = adjoint_update(dat, parameters, deps),
    compare = adjoint_compare(dat, parameters, deps),
    initial = adjoint_initial(dat, parameters, deps))

  ## Update storage with all this information:
  adjoint_location <- merge_location(lapply(dat$adjoint, "[[", "location"))
  dat$storage$contents$adjoint <-
    names(adjoint_location)[adjoint_location == "adjoint"]
  dat$storage$location <- c(dat$storage$location, adjoint_location)
  dat$storage$packing$adjoint <-
    list(scalar = dat$storage$contents$adjoint)
  dat$storage$type <- c(
    dat$storage$type,
    set_names(rep("real_type", length(dat$storage$contents$adjoint)),
              dat$storage$contents$adjoint))

  dat
}


adjoint_update <- function(dat, parameters, deps) {
  update <- dat$phases$update
  used <- adjoint_uses(update$variables, update$equations, deps)
  equations <- c(update$variables, dat$equations[used])
  eqs <- c(
    lapply(used, adjoint_equation, equations,
           intermediate = TRUE, accumulate = FALSE),
    lapply(dat$variables, adjoint_equation, equations,
           intermediate = FALSE, accumulate = FALSE),
    lapply(parameters, adjoint_equation, equations,
           intermediate = FALSE, accumulate = TRUE))
  adjoint_phase(eqs, dat)
}


adjoint_compare <- function(dat, parameters, deps) {
  compare <- dat$phases$compare
  used <- adjoint_uses(compare$compare, compare$equations, deps)
  equations <- c(compare$variables, compare$compare, dat$equations[used])
  eqs <- c(
    lapply(used, adjoint_equation, equations,
           intermediate = TRUE, accumulate = FALSE),
    lapply(dat$variables, adjoint_equation, equations,
           intermediate = FALSE, accumulate = TRUE),
    lapply(parameters, adjoint_equation, equations,
           intermediate = FALSE, accumulate = TRUE))
  adjoint_phase(eqs, dat)
}


adjoint_initial <- function(dat, parameters, deps) {
  initial <- dat$phases$initial
  used <- adjoint_uses(initial$variables, initial$equations, deps)
  equations <- c(initial$variables, initial$initial, dat$equations[used])
  eqs <- c(
    lapply(used, adjoint_equation, equations,
           intermediate = TRUE, accumulate = FALSE),
    lapply(dat$variables, adjoint_equation, equations,
           intermediate = FALSE, accumulate = TRUE),
    lapply(parameters, adjoint_equation, equations,
           intermediate = FALSE, accumulate = TRUE))
  adjoint_phase(eqs, dat)
}


adjoint_uses <- function(phase, equations, deps) {
  used <- unique(unlist0(lapply(phase, function(eq) eq$rhs$depends$variables)))
  used <- c(used, equations)
  used <- union(used, unlist0(deps[equations]))
  rev(intersect(equations, used))
}


adjoint_phase <- function(eqs, dat) {
  uses <- unique(
    unlist0(lapply(eqs, function(x) find_dependencies(x)$variables)))
  unpack <- intersect(dat$variables, uses)
  ## Alternatively, filter *to* things in stack/internal?
  ignore <- c(dat$storage$contents$shared,
              dat$storage$contents$data,
              dat$variables)
  equations <- setdiff(intersect(names(dat$equations), uses), ignore)
  location <- set_names(vcapply(eqs, function(x) x$lhs$location),
                        vcapply(eqs, function(x) x$lhs$name))
  list(unpack = unpack,
       equations = equations,
       adjoint = eqs,
       location = location)
}


adjoint_equation <- function(nm, equations, intermediate, accumulate) {
  prefix <- "adj_" # we might move this elsewhere?

  ## We can apply this approach and at the *moment* everything we
  ## create goes on the stack.  Later we will end up with non scalars
  ## though which go on the heap.
  i <- vlapply(equations, function(x) nm %in% x$rhs$depends$variables)
  f <- function(eq) {
    if (identical(eq$special, "compare")) {
      differentiate(log_density(eq$rhs$distribution, eq$rhs$args), nm)
    } else {
      maths$times(as.name(paste0(prefix, eq$lhs$name)),
                  differentiate(eq$rhs$expr, nm))
    }
  }
  expr <- fold_add(lapply(equations[i], f))
  location <- if (intermediate) "stack" else "adjoint"
  list(adjoint = TRUE,
       lhs = list(name = paste0(prefix, nm),
                  location = location),
       rhs = list(type = "expression",
                  expr = expr,
                  accumulate = accumulate))
}


merge_location <- function(x) {
  ret <- x[[1]]
  for (el in x[-1]) {
    shared <- intersect(names(el), names(ret))
    stopifnot(identical(el[shared], ret[shared]))
    ret <- c(ret, el[setdiff(names(el), names(ret))])
  }
  ret
}
