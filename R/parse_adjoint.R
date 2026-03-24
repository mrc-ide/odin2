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

  ## Not sure what I will need to do here, but it's a bunch!
  if (nrow(dat$storage$arrays) > 0) {
    stop("Implement differentiation with arrays") # nocov
  }
  arrays <- NULL

  ## TODO: validate that we have data/compare because otherwise we
  ## have nothing to differentiate!

  deps <- lapply(dat$equations, function(eq) eq$rhs$depends$variables_recursive)

  dat$adjoint <- list(
    update = adjoint_update(dat, parameters, deps),
    compare = adjoint_compare(dat, parameters, deps),
    initial = adjoint_initial(dat, parameters, deps))

  ## For continuous models, generate adjoint_rhs (Jacobian transpose of
  ## the RHS).  This is used by dust_continuous::adjoint_run_to_time()
  ## to integrate the adjoint ODE backward between data times.
  if (dat$time == "continuous") {
    dat$adjoint$deriv <- adjoint_deriv(dat, parameters, deps)
  }

  ## Update storage with all this information
  adjoint_location <- merge_location(lapply(dat$adjoint, "[[", "location"))
  for (nm in names(dat$adjoint)) {
    dat$adjoint$location <- NULL
  }

  ## Force state adjoints before parameter adjoints in packing to match
  ## the layout expected by adjoint_data::gradient() (which reads
  ## parameter gradients from offset n_state onwards).
  adj_state_names <- paste0("adj_", dat$variables)
  adj_param_names <- paste0("adj_", parameters)
  all_adj_ordered <- c(adj_state_names, adj_param_names)
  all_adj_ordered <- intersect(
    all_adj_ordered,
    names(adjoint_location)[adjoint_location == "adjoint"])
  dat$storage$contents$adjoint <- all_adj_ordered
  dat$storage$location <- c(dat$storage$location, adjoint_location)
  dat$storage$packing$adjoint <-
    parse_packing(dat$storage$contents$adjoint, arrays, NULL, "adjoint")
  dat$storage$packing$gradient <-
    parse_packing(parameters, arrays, NULL, "gradient")

  dat$storage$type <- c(
    dat$storage$type,
    set_names(rep("real_type", length(adjoint_location)),
              names(adjoint_location)))

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


## Generate adjoint equations for the RHS (deriv phase) of continuous
## models.  The adjoint ODE is dλ/dt = -(∂f/∂y)^T λ with parameter
## accumulation dμ/dt = -(∂f/∂θ)^T λ.  We use the same differentiation
## machinery as adjoint_update but applied to the deriv equations.
adjoint_deriv <- function(dat, parameters, deps) {
  deriv <- dat$phases$deriv
  used <- adjoint_uses(deriv$variables, deriv$equations, deps)
  equations <- c(deriv$variables, dat$equations[used])
  eqs <- c(
    lapply(used, adjoint_equation, equations,
           intermediate = TRUE, accumulate = FALSE),
    lapply(dat$variables, adjoint_equation, equations,
           intermediate = FALSE, accumulate = FALSE),
    lapply(parameters, adjoint_equation, equations,
           intermediate = FALSE, accumulate = FALSE))
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
    unlist0(lapply(eqs, function(x) find_dependencies(x$rhs$expr)$variables)))

  ## This is a bit gross; we need to find all variables referenced in
  ## all equations referenced in all adjoint calculations!
  uses_in_equations <- unlist0(
    lapply(dat$equations[intersect(names(dat$equations), uses)],
           function(x) x$rhs$depends$variables))
  uses <- union(uses, uses_in_equations)

  unpack <- intersect(dat$variables, uses)
  ## Alternatively, filter *to* things in stack/internal?
  ignore <- c(dat$storage$contents$shared,
              dat$storage$contents$data,
              dat$variables)
  equations <- setdiff(intersect(names(dat$equations), uses), ignore)
  location <- set_names(vcapply(eqs, function(x) x$lhs$location),
                        vcapply(eqs, function(x) x$lhs$name))
  ## Always include adjoint-location equations (they are outputs);
  ## only filter out unused stack (intermediate) equations.
  include_adjoint <- (names(location) %in% uses) | (location != "stack")

  unpack_adjoint <- intersect(names(location)[location == "adjoint"], uses)
  list(unpack = unpack,
       unpack_adjoint = unpack_adjoint,
       equations = equations,
       adjoint = eqs[include_adjoint],
       location = location[include_adjoint])
}


adjoint_equation <- function(nm, equations, intermediate, accumulate) {
  prefix <- "adj_" # we might move this elsewhere?
  diff <- monty::monty_differentiation()
  differentiate <- diff$differentiate
  maths <- diff$maths

  ## We can apply this approach and at the *moment* everything we
  ## create goes on the stack.  Later we will end up with non scalars
  ## though which go on the heap.
  i <- vlapply(equations, function(x) nm %in% x$rhs$depends$variables)
  f <- function(eq) {
    if (rlang::is_call(eq$src$value, "~")) {
      differentiate(eq$rhs$density$expr, nm)
    } else {
      if (isTRUE(eq$rhs$is_stochastic)) {
        expr <- rewrite_stochastic_to_expectation(eq$rhs$expr)
      } else {
        expr <- eq$rhs$expr
      }
      maths$times(as.name(paste0(prefix, eq$lhs$name)),
                  differentiate(expr, nm))
    }
  }

  name <- paste0(prefix, nm)
  parts <- lapply(equations[i], f)
  if (accumulate) {
    parts <- c(parts, list(as.name(name)))
  }
  expr <- maths$plus_fold(parts)
  location <- if (intermediate) "stack" else "adjoint"
  list(lhs = list(name = name,
                  location = location),
       rhs = list(type = "expression",
                  expr = expr))
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
