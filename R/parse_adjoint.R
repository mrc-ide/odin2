parse_adjoint <- function(dat) {
  ## We need to make sure that we can work with this system; that
  ## requires that we have at least one bit of comparison to data and
  ## at least one differentiable parameter).
  equations <- dat$equations
  variables <- dat$location$contents$variables
  parameters <- dat$parameters$name[dat$parameters$differentiate]

  ## It's possible that we get collisions this way, let's find out:
  prefix <- "adj_"

  ## There's some common bits that we can just write out: we are going
  ## to require adjoint storage for the variables and parameters:
  nms <- paste0(prefix, c(variables, parameters))
  dat$location$contents$adjoint <- nms
  packing <- list(
    adjoint = set_names(as.list(seq_along(nms) - 1), nms))
  dat$location$location <- c(
    dat$location$location,
    set_names(rep("adjoint", length(nms)), nms))
  dat$location$type <- c(
    dat$location$location,
    set_names(rep("real_type", length(nms)), nms))
    
  ## So we really are going to end up with a new model out of this,
  ## but one that uses the components of a parent model probably (in
  ## its parameters etc), because we are eventually going to create
  ## more temporary storage, and that does not really want to go into
  ## the main model.
  update <- dat$phases$update
  used <- union(
    update$equations,
    unlist(lapply(equations[update$equations],
                  function(x) x$rhs$depends$variables), FALSE, FALSE))
  used_equations <- intersect(names(equations), used)
  used_equations <- used_equations[
    vcapply(equations[used_equations], function(x) x$stage) == "time"]

  equations_all <- c(update$variables, equations)
  eqs_update <- c(
    lapply(rev(used_equations), adjoint_equation, equations_all, prefix),
    lapply(variables, adjoint_equation, equations_all, prefix),
    lapply(parameters, adjoint_equation, equations_all, prefix,
           accumulate = TRUE))

  used_adjoint <- unique(unlist(
    lapply(eqs_update, function(x) x$rhs$depends$variables),
    FALSE, FALSE))
  equations_time <-
    names(equations)[vcapply(equations, function(x) x$stage) == "time"]
  dat$phases$adjoint_update <- list(
    unpack = intersect(variables, used_adjoint),
    equations = intersect(equations_time, used_adjoint),
    adjoint = eqs_update)

  browser()
  ## Now, also for the comparison:
  compare <- dat$phases$compare
  used <- union(
    compare$equations,
    unlist(lapply(equations[compare$equations],
                  function(x) x$rhs$depends$variables), FALSE, FALSE))
  used_equations <- intersect(names(equations), used)
  used_equations <- used_equations[
    vcapply(equations[used_equations], function(x) x$stage) == "time"]

  ## I'm definitely incorrect here, as I am pulling in equations for
  ## the initial update which is just wrong.  I think that this is
  ## where I have the role wrong?  We seem to be particularly
  ## incorrect for adj_I, which is just pulling in all sorts?

  ## adj_p_SI * (beta * dt) * exp(-beta * I * dt/N)/N + adj_n_IR * p_IR

  ## 

  
  
  
  equations_all <- c(compare$compare, equations)
  eqs_compare <- c(
    lapply(rev(used_equations), adjoint_equation, equations_all, prefix),
    lapply(variables, adjoint_equation, equations_all, prefix),
    lapply(parameters, adjoint_equation, equations_all, prefix,
           accumulate = TRUE))


  dat
}


adjoint_equation <- function(nm, equations, prefix, accumulate = FALSE) {
  ## We can apply this approach and at the *moment* everything we
  ## create goes on the stack.  Later we will end up with non scalars
  ## though which go on the heap.
  i <- vlapply(equations, function(x) nm %in% x$rhs$depends$variables)
  f <- function(eq) {
    if (identical(eq$special, "compare")) {
      differentiate(log_density(eq$rhs$expr), nm)
    } else {
      maths$times(as.name(paste0(prefix, eq$lhs$name)),
                  differentiate(eq$rhs$expr, nm))
    }
  }
  parts <- lapply(equations[i], f)
  if (accumulate) {
    parts <- c(parts, list(as.name(paste0(prefix, nm))))
  }
  expr <- fold_add(parts)
  list(name = paste0(prefix, nm),
       rhs = list(expr = expr,
                  depends = find_dependencies(expr)))
}
