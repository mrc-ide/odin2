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

  ## Build array metadata for adjoint variables.  Each array state

  ## variable X gets a corresponding adj_X with the same dimensions.
  ## Scalar parameters that are differentiated do not need array info.
  arrays <- dat$storage$arrays
  if (nrow(arrays) > 0) {
    adj_array_rows <- arrays[arrays$name %in% dat$variables, , drop = FALSE]
    if (nrow(adj_array_rows) > 0) {
      adj_arrays <- adj_array_rows
      adj_arrays$name <- paste0("adj_", adj_arrays$name)
      ## Alias to the original variable so dim lookups resolve correctly
      ## (adj_S uses dim.S, not dim.adj_S which doesn't exist)
      arrays <- rbind(arrays, adj_arrays)
    }
  }

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

  ## Find adjoint variables stored in "internal" — these are array
  ## intermediates whose adjoints also need array storage.  Add them
  ## to the internal contents and the arrays table.
  adj_internal_names <- names(adjoint_location)[adjoint_location == "internal"]
  if (length(adj_internal_names) > 0) {
    dat$storage$contents$internal <- c(dat$storage$contents$internal,
                                       adj_internal_names)
    ## Build array rows for these adjoint internal arrays, aliased to
    ## the forward counterpart's dimensions.
    for (adj_nm in adj_internal_names) {
      fwd_nm <- sub("^adj_", "", adj_nm)
      fwd_i <- match(fwd_nm, arrays$name)
      if (!is.na(fwd_i) && !(adj_nm %in% arrays$name)) {
        new_row <- arrays[fwd_i, , drop = FALSE]
        new_row$name <- adj_nm
        ## alias stays as original so dim lookups go to dim.fwd_nm
        arrays <- rbind(arrays, new_row)
      }
    }
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
  ## Prevent parse_packing from reordering: state adjoints MUST come
  ## before parameter adjoints to match dust2::adjoint_data layout
  ## (gradient is read from offset n_state onwards).
  dat$storage$packing$adjoint <-
    parse_packing(dat$storage$contents$adjoint, arrays,
                  all_adj_ordered, "adjoint")
  dat$storage$packing$gradient <-
    parse_packing(parameters, arrays, NULL, "gradient")

  dat$storage$type <- c(
    dat$storage$type,
    set_names(rep("real_type", length(adjoint_location)),
              names(adjoint_location)))

  ## Update arrays table so code generation sees adjoint array info
  dat$storage$arrays <- arrays

  dat
}


adjoint_update <- function(dat, parameters, deps) {
  update <- dat$phases$update
  arrays <- dat$storage$arrays
  used <- adjoint_uses(update$variables, update$equations, deps)
  equations <- c(update$variables, dat$equations[used])
  eqs <- c(
    lapply(used, adjoint_equation, equations,
           intermediate = TRUE, accumulate = FALSE, arrays = arrays),
    lapply(dat$variables, adjoint_equation, equations,
           intermediate = FALSE, accumulate = FALSE, arrays = arrays),
    lapply(parameters, adjoint_equation, equations,
           intermediate = FALSE, accumulate = TRUE, arrays = arrays))
  adjoint_phase(eqs, dat)
}


adjoint_compare <- function(dat, parameters, deps) {
  compare <- dat$phases$compare
  arrays <- dat$storage$arrays
  used <- adjoint_uses(compare$compare, compare$equations, deps)
  equations <- c(compare$variables, compare$compare, dat$equations[used])
  eqs <- c(
    lapply(used, adjoint_equation, equations,
           intermediate = TRUE, accumulate = FALSE, arrays = arrays),
    lapply(dat$variables, adjoint_equation, equations,
           intermediate = FALSE, accumulate = TRUE, arrays = arrays),
    lapply(parameters, adjoint_equation, equations,
           intermediate = FALSE, accumulate = TRUE, arrays = arrays))
  adjoint_phase(eqs, dat)
}


adjoint_initial <- function(dat, parameters, deps) {
  initial <- dat$phases$initial
  arrays <- dat$storage$arrays
  used <- adjoint_uses(initial$variables, initial$equations, deps)
  equations <- c(initial$variables, initial$initial, dat$equations[used])
  eqs <- c(
    lapply(used, adjoint_equation, equations,
           intermediate = TRUE, accumulate = FALSE, arrays = arrays),
    lapply(dat$variables, adjoint_equation, equations,
           intermediate = FALSE, accumulate = TRUE, arrays = arrays),
    lapply(parameters, adjoint_equation, equations,
           intermediate = FALSE, accumulate = TRUE, arrays = arrays))
  adjoint_phase(eqs, dat)
}


## Generate adjoint equations for the RHS (deriv phase) of continuous
## models.  The adjoint ODE is dλ/dt = -(∂f/∂y)^T λ with parameter
## accumulation dμ/dt = -(∂f/∂θ)^T λ.  We use the same differentiation
## machinery as adjoint_update but applied to the deriv equations.
adjoint_deriv <- function(dat, parameters, deps) {
  deriv <- dat$phases$deriv
  arrays <- dat$storage$arrays
  used <- adjoint_uses(deriv$variables, deriv$equations, deps)
  equations <- c(deriv$variables, dat$equations[used])
  eqs <- c(
    lapply(used, adjoint_equation, equations,
           intermediate = TRUE, accumulate = FALSE, arrays = arrays),
    lapply(dat$variables, adjoint_equation, equations,
           intermediate = FALSE, accumulate = FALSE, arrays = arrays),
    lapply(parameters, adjoint_equation, equations,
           intermediate = FALSE, accumulate = FALSE, arrays = arrays))
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
  ## Also exclude dim_* equations (staged to build_shared only) and
  ## any equation staged to "create" — these are setup-only.
  dim_eqs <- grep("^dim_", names(dat$equations), value = TRUE)
  ignore <- c(dat$storage$contents$shared,
              dat$storage$contents$data,
              dat$variables,
              dim_eqs)
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


adjoint_equation <- function(nm, equations, intermediate, accumulate,
                            arrays = NULL) {
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
      ## Unwrap odin-internal representations (OdinReduce, etc.)
      ## back to forms the differentiator understands.
      expr <- adjoint_unwrap_expr(expr)
      ## Build the adjoint reference.  For array equations, the
      ## adjoint of the LHS is indexed with the same loop variables.
      adj_name <- paste0(prefix, eq$lhs$name)
      if (!is.null(eq$lhs$array)) {
        idx <- lapply(eq$lhs$array, function(a) {
          if (a$type == "range") as.name(a$name) else a$at
        })
        adj_ref <- as.call(c(list(as.name("["), as.name(adj_name)), idx))
      } else {
        adj_ref <- as.name(adj_name)
      }
      maths$times(adj_ref, differentiate(expr, nm))
    }
  }

  name <- paste0(prefix, nm)
  parts <- lapply(equations[i], f)

  ## Determine if the adjoint variable is an array.  This happens
  ## when nm is an array state variable.  The adjoint equation then
  ## inherits the same loop structure.
  lhs_array <- NULL
  if (!is.null(arrays) && nm %in% arrays$name) {
    ## First try: get array info from forward equations in this phase.
    array_eqs <- equations[vlapply(equations, function(x) {
      identical(x$lhs$name, nm) && !is.null(x$lhs$array)
    })]
    if (length(array_eqs) > 0) {
      lhs_array <- array_eqs[[1]]$lhs$array
    } else {
      ## Fallback: build generic range loop info from the arrays table.
      ## This handles phases (compare, initial) where the variable's
      ## own equation isn't present.
      arr_i <- match(nm, arrays$name)
      arr_rank <- arrays$rank[[arr_i]]
      idx_names <- c("i", "j", "k", "l", "i5", "i6", "i7", "i8")
      lhs_array <- lapply(seq_len(arr_rank), function(d) {
        list(name = idx_names[d],
             type = "range",
             from = 1,
             to = call("OdinDim", nm, as.integer(d)))
      })
    }
  }

  if (accumulate) {
    ## For array adjoints, the accumulate reference must be indexed:
    ## adj_S[i] instead of bare adj_S.
    if (!is.null(lhs_array)) {
      idx <- lapply(lhs_array, function(a) {
        if (a$type == "range") as.name(a$name) else a$at
      })
      accum_ref <- as.call(c(list(as.name("["), as.name(name)), idx))
    } else {
      accum_ref <- as.name(name)
    }
    parts <- c(parts, list(accum_ref))
  }
  expr <- maths$plus_fold(parts)

  ## Re-wrap any bare sum() calls back to OdinReduce for code generation.
  if (!is.null(arrays) && nrow(arrays) > 0) {
    expr <- adjoint_rewrap_sums(expr, arrays)
  }

  location <- if (intermediate) {
    ## Array intermediate adjoints can't go on the stack — use internal
    ## storage (like the forward intermediates themselves).
    if (!is.null(lhs_array)) "internal" else "stack"
  } else {
    "adjoint"
  }

  ## For scalar adjoint variables whose expression uses array loop
  ## indices, we need a reduction loop.  Detect this by checking if
  ## any contributing equation is an array equation.
  reduce_loops <- NULL
  if (is.null(lhs_array) && any(i)) {
    array_contributors <- equations[i][vlapply(equations[i], function(x) {
      !is.null(x$lhs$array)
    })]
    if (length(array_contributors) > 0) {
      reduce_loops <- array_contributors[[1]]$lhs$array
    }
  }

  list(lhs = list(name = name,
                  location = location,
                  array = lhs_array),
       rhs = list(type = "expression",
                  expr = expr),
       reduce_loops = reduce_loops)
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


## Recursively replace odin-internal function calls (OdinReduce, etc.)
## with forms that monty::monty_differentiation() can handle.
## OdinReduce(fn="sum", ..., expr=sum(X[])) -> sum(X[])
adjoint_unwrap_expr <- function(expr) {
  if (is.numeric(expr) || is.symbol(expr) || is.character(expr)) {
    return(expr)
  }
  if (rlang::is_call(expr, "OdinReduce")) {
    return(expr[["expr"]])
  }
  as.call(lapply(expr, adjoint_unwrap_expr))
}


## Reverse of adjoint_unwrap_expr: convert bare sum(X) or sum(X[...])
## back to OdinReduce so the C++ code generator can emit them correctly.
## This runs AFTER differentiation on the adjoint expressions.
adjoint_rewrap_sums <- function(expr, arrays) {
  if (is.null(expr) || is.numeric(expr) || is.symbol(expr) ||
      is.character(expr) || is.logical(expr)) {
    return(expr)
  }
  ## Don't descend into OdinReduce — already in correct form
  if (rlang::is_call(expr, "OdinReduce")) {
    return(expr)
  }
  if (rlang::is_call(expr, "sum") && length(expr) == 2) {
    arg <- expr[[2]]
    ## sum(X) where X is a bare symbol (whole-array sum)
    if (is.symbol(arg)) {
      nm <- as.character(arg)
      if (nm %in% arrays$name) {
        return(adjoint_make_reduce("sum", nm, index = NULL, expr = expr))
      }
    }
    ## sum(X[i, ]) or sum(X[, j]) — partial sum with indexing
    if (rlang::is_call(arg, "[")) {
      nm <- as.character(arg[[2]])
      if (nm %in% arrays$name) {
        idx <- as.list(arg[-(1:2)])
        arr_i <- match(nm, arrays$name)
        rank <- arrays$rank[[arr_i]]
        idx_names <- c("i", "j", "k", "l", "i5", "i6", "i7", "i8")
        ## Build index info for each dimension
        has_index <- FALSE
        index <- lapply(seq_len(rank), function(d) {
          if (d > length(idx) || rlang::is_missing(idx[[d]])) {
            ## Full range: 1 to dim(X, d)
            list(name = idx_names[d], type = "range",
                 from = 1L,
                 to = call("OdinDim", nm, as.integer(d)))
          } else {
            has_index <<- TRUE
            list(name = idx_names[d], type = "single", at = idx[[d]])
          }
        })
        ## If all NULL, it's a whole-array sum
        if (!has_index) {
          return(adjoint_make_reduce("sum", nm, index = NULL, expr = expr))
        }
        return(adjoint_make_reduce("sum", nm, index = index, expr = expr))
      }
    }
  }
  ## Recursively process call arguments, preserving NULL elements
  args <- as.list(expr)
  for (i in seq_along(args)) {
    if (!is.null(args[[i]])) {
      args[[i]] <- adjoint_rewrap_sums(args[[i]], arrays)
    }
  }
  as.call(args)
}


adjoint_make_reduce <- function(fn, what, index, expr) {
  call("OdinReduce", fn = fn, what = what, index = index, expr = expr)
}
