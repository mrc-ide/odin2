parse_array_bounds <- function(dat, call) {
  ## Find all array accesses, read or write, across all our equations:
  constraints <- parse_array_bounds_extract_constraints(dat)
  if (is.null(constraints)) {
    return()
  }

  ## We try and resolve dimensions as much as possible before going
  ## too far.  Try to resolve through equations until we find either
  ## constants or parameters, as a list:
  arrays <- constraint_resolve_dimensions(dat$storage$arrays, dat$equations,
                                          dat$variables)

  ## Then we want to sort these out into provably ok, provably bad and
  ## unknown.  We will do this element by element:
  res <- lapply(split(constraints, constraints$name), constraint_triage,
                arrays, dat$equations, dat$variables, dat$src, call)


  ## At this point we still have lots of repetition among constraints
  ## that we will jhave found; it's likely that we will have n <= EXPR
  ## all over the show, and we can collect these together.
  res <- constraint_finalise(rlang::inject(rbind(!!!unname(res))))

  ## We might be able to remove some parameter constraints by looking
  ## at the min values that they take (if given).  We should also
  ## report back on accesses not proven, but that also can wait as it
  ## should be possible to action or silence these all.
  ##
  ## Some of these from sircovid/lancelot are really quite hard, so
  ## this stage is nontrivial
  NULL
}


parse_array_bounds_extract_constraints <- function(dat) {
  arrays <- dat$storage$arrays
  if (nrow(arrays) == 0) {
    return(NULL)
  }

  check <- c(
    dat$equations,
    dat$phases$update$variables,
    dat$phases$deriv$variables,
    dat$phases$output$variables,
    dat$phases$initial$variables,
    dat$phases$compare$compare)

  ## Find all equations that use an array - either as an assignment
  ## into arrays (in which case we have lhs$array) or by referencing
  ## an array (in which case we could either look for '[' in the
  ## functions or arrays within dependencies).  Look at the value used
  ## to index into an array and compare that with the size of that
  ## dimension of the array.  Don't try and be clever yet, there's
  ## enough to be done here; this stage just finds the accesses:
  constraints <- unlist0(lapply(check, parse_array_bounds_extract_constraint))

  ## Convert this list-of-lists into a nicer data frame:
  constraints <- data.frame(
    name = vcapply(constraints, "[[", "name"),
    expr = I(lapply(constraints, "[[", "expr")),
    dimension = viapply(constraints, "[[", "dimension"),
    at = I(lapply(constraints, "[[", "at")),
    src = viapply(constraints, "[[", "src"),
    write = vlapply(constraints, "[[", "write"))

  ## Sort and deduplicate the worst of the repetition:
  constraints <- constraints[order(constraints$name, !constraints$write), ]
  id <- sprintf("%s:%s", constraints$name, vcapply(constraints$at, rlang::hash))
  constraints <- constraints[!duplicated(id), ]
  rownames(constraints) <- NULL
  constraints
}


parse_array_bounds_extract_constraint <- function(eq) {
  c(parse_array_bounds_extract_constraint_lhs(eq),
    parse_array_bounds_extract_constraint_rhs(eq))
}


parse_array_bounds_extract_constraint_lhs <- function(eq) {
  ret <- collector(list())
  if (!is.null(eq$lhs$array)) {
    name <- eq$lhs$name
    expr <- eq$src$value[[2]]
    for (i in seq_along(eq$lhs$array)) {
      idx <- eq$lhs$array[[i]]
      for (v in c("from", "to", "at")) {
        if (!is.null(idx[[v]])) {
          ret$add(constraint(name, expr, i, idx[[v]], eq$src$index, TRUE))
        }
      }
    }
  }
  ret$get()
}


parse_array_bounds_extract_constraint_rhs <- function(eq) {
  warn_unhandled_analysis <- function(expr, uses) {
    ## We get here in cases where both i and j appear within an array
    ## access such as:
    ##
    ## > x[if (condition) i else j]
    ## > x[i + j * n]
    ##
    ## Just ignore these for now, but later we might want to provide a
    ## warning indicating that these were not considered.  Quite a bit
    ## more analysis would be required to sort this out unfortunately.
    cli::cli_warn(
      c(paste("Cannot validate array access '{deparse1(expr)}' using",
              "more than one index variable ({squote(uses)})"),
        ">" = "Context:",
        format_src(parse_error_src(eq$src))))
  }

  if (!("[" %in% eq$rhs$depends$functions)) {
    return(NULL)
  }

  ret <- collector(list())
  extract <- function(expr) {
    if (rlang::is_call(expr, "[")) {
      name <- deparse(expr[[2]])
      at <- as.list(expr[-(1:2)])
      for (i in seq_along(at)) {
        uses <- intersect(all.vars(at[[i]]), INDEX)
        if (length(uses) == 0) {
          ret$add(constraint(name, expr, i, at[[i]], eq$src$index, FALSE))
        } else if (length(uses) == 1) {
          idx <- eq$lhs$array[[match(uses, INDEX)]]
          for (v in c("from", "to", "at")) {
            if (!is.null(idx[[v]])) {
              at_i <- substitute_(at[[i]], set_names(idx[v], uses))
              ret$add(constraint(name, expr, i, at_i, eq$src$index, FALSE))
            }
          }
        } else {
          warn_unhandled_analysis(expr, uses)
        }
      }
    } else if (is.recursive(expr)) {
      lapply(expr[-1], extract)
    }
  }
  extract(eq$rhs$expr)
  ret$get()
}


constraint <- function(name, expr, dimension, at, src, write) {
  ## We'll enforce elsewhere that all arrays have length 1 or more,
  ## not sure where though!
  if (is.numeric(at) && at == 1) {
    return(NULL)
  }
  list(
    list(
      name = name, expr = expr, dimension = dimension, at = at, src = src,
      write = write))
}


## At this point we only have the constraints for a single array
constraint_triage <- function(d, arrays, equations, variables, src, call) {
  throw_bad_access <- function(i) {
    src_index <- d$src[[i]]
    rank <- length(arrays[[name]])
    dim <- d$dimension[[i]]
    at_value <- at[[i]]
    size_value <- size[[i]]
    expr <- deparse1(d$expr[[i]])
    access <- if (d$write[[i]]) "write" else "read"

    if (rank == 1) {
      msg <- "Out of range {access} of '{name}' in '{expr}'"
      hint_size <- "'{name}' has capacity: {constraint_deparse(size_value)}"
    } else {
      msg <- "Out of range {access} of '{name}' in dimension {dim} of '{expr}'"
      hint_size <- "'Dimension {dim} of {name}' has capacity: {size_value}"
    }
    hint_access <- "Trying to {access} element: {constraint_deparse(at_value)}"

    odin_parse_error(
      c(msg,
        i = hint_size,
        x = hint_access),
      "E3001", src[[src_index]], call)
  }

  name <- d$name[[1]]
  rank <- length(arrays[[name]])
  at <- lapply(d$at, constraint_simplify_expr, arrays, equations, variables)
  size <- arrays[[name]][d$dimension]
  result <- Map(constraint_solve, at, size)

  valid <- vlapply(result, "[[", "valid")
  err <- !is.na(valid) & !valid

  if (any(err)) {
    throw_bad_access(which(err)[[1]])
  }

  if (!anyNA(valid)) {
    return(NULL)
  }

  i <- is.na(valid)
  ret <- d[i, ]
  ret$constraint <- I(lapply(result[i], "[[", "constraint"))
  rownames(ret) <- NULL
  ret
}


constraint_solve <- function(at, size) {
  if (is.numeric(at) && is.numeric(size)) {
    return(list(valid = at <= size))
  }
  if (identical(at, size)) {
    return(list(valid = TRUE))
  }
  maths <- monty::monty_differentiation()$maths

  at <- maths$plus_fold(lapply(expr_to_sum_of_parts(at), maths$uminus))
  expr <- expr_factorise(maths$plus(size, at))

  if (is.numeric(expr)) {
    return(list(valid = expr >= 0))
  }

  list(valid = NA, constraint = constraint_tidy(expr))
}


constraint_resolve_dimensions <- function(arrays, equations, variables) {
  is_alias <- arrays$alias != arrays$name
  dims <- set_names(arrays$dims[!is_alias], arrays$name[!is_alias])

  ## Placeholder for
  ##
  ## dim(x) <- parameter(rank = n)
  ##
  ## which are all basically hopeless anyway except for dynamic checks.
  i <- vlapply(dims, function(x) all(vlapply(x, is.null)))
  if (any(i)) {
    dims[i] <- lapply(names(dims)[i], function(nm) {
      lapply(seq_along(dims[[nm]]), function(i) call("OdinParameterDim", nm, i))
    })
  }

  dims <- lapply(dims, lapply, constraint_simplify_expr,
                 dims, equations, variables)

  ret <- set_names(vector("list", nrow(arrays)), arrays$name)
  ret[!is_alias] <- dims
  ret[is_alias] <- ret[arrays$alias[is_alias]]
  ret
}


constraint_simplify_expr <- function(expr, arrays, equations, variables) {
  maths <- monty::monty_differentiation()$maths
  unknowable <- c("OdinParameter", "OdinParameterDim", "OdinVariable")
  simplify <- function(x) {
    if (is.numeric(x)) {
      return(x)
    } else if (is.symbol(x)) {
      nm <- as.character(x)
      if (nm %in% variables) {
        return(call("OdinVariable", nm))
      }
      stopifnot(sum(names(equations) == nm) == 1)
      eq <- equations[[nm]]
      if (is.null(eq$special)) {
        simplify(eq$rhs$expr)
      } else if (eq$special == "parameter") {
        call("OdinParameter", nm)
      } else {
        stop("[odin bug] Unexpected equation type during simplification")
      }
    } else if (rlang::is_call(x, c("as.numeric", "as.integer"))) {
      simplify(x[[2]])
    } else if (rlang::is_call(x, "OdinDim")) {
      nm <- x[[2]]
      i <- x[[3]]
      return(simplify(arrays[[nm]][[i]]))
    } else if (rlang::is_call(x, c("length", "OdinLength", "nrow"))) {
      nm <- as.character(x[[2]])
      return(simplify(arrays[[nm]][[1]]))
    } else if (rlang::is_call(x, "ncol")) {
      nm <- as.character(x[[2]])
      return(simplify(arrays[[nm]][[2]]))
    } else if (rlang::is_call(x, unknowable)) {
      return(x)
    } else if (is.recursive(x)) {
      x[-1] <- lapply(x[-1], simplify)
      maths$rewrite(x)
    } else {
      stop("[odin bug] Unexpected value during simplification")
    }
  }

  simplify(expr)
}


## Here, we have our final list of constraints.  But there's still
## more that can be done!  Many of these will be of the form
##
## > OdinParameter("name") >= EXPR
##
## and where EXPR is numeric we only need to find the largest EXPR.
##
## Later, we could also look to see how min/max constraints on
## parameters interact here, and to try and deal with any that involve
## two parameters.
constraint_finalise <- function(constraints) {
  if (length(constraints) == 0) {
    return(NULL)
  }
  constraints <- constraints[order(constraints$write, constraints$name), ]
  constraints <- constraints[!duplicated(constraints$constraint), ]


  constraints$parameter <- vcapply(constraints$constraint, function(expr) {
    if (rlang::is_call(expr[[2]], "OdinParameter")) {
      expr[[2]][[2]]
    } else {
      NA_character_
    }
  })

  i <- !is.na(constraints$parameter)
  if (any(i)) {
    ## Split by parameter, and sum all numeric constraints over
    ## parameter
    dat <- split(constraints[i, ], constraints$parameter[i])
    dat <- lapply(dat, function(d) {
      v <- vnapply(d$constraint, function(expr) {
        if (rlang::is_scalar_integerish(expr[[3]])) expr[[3]] else NA_real_
      })
      j <- !is.na(v)
      if (sum(j) > 1) d[!(j & v < max(v[j])), ] else d
    })
    constraints <- rbind(constraints[!i, ],
                         rlang::inject(rbind(!!!unname(dat))))
  }

  rownames(constraints) <- NULL
  constraints
}


constraint_tidy <- function(expr) {
  if ("OdinParameter" %in% all.names(expr)) {
    maths <- monty::monty_differentiation()$maths
    parts <- expr_to_sum_of_parts(expr)
    i <- vlapply(parts, rlang::is_call, "OdinParameter")
    call(">=",
         maths$plus_fold(parts[i]),
         maths$plus_fold(lapply(parts[!i], maths$uminus)))
  } else {
    call(">=", expr, 0)
  }
}


constraint_deparse <- function(expr, arrays) {
  deparse1(constraint_deparse_rewrite(expr, arrays))
}


constraint_deparse_rewrite <- function(expr, arrays) {
  if (rlang::is_call(expr, c("OdinParameter", "OdinVariable"))) {
    as.name(expr[[2]])
  } else if (rlang::is_call(expr, "OdinDim")) {
    name <- expr[[2]]
    rank <- length(arrays[[name]])
    dim <- expr[[3]]
    sym <- as.name(name)
    if (rank == 1) {
      call("length", sym)
    } else if (rank == 2) {
      call(if (dim == 1) "nrow" else "ncol", sym)
    } else {
      ## TODO: need a way of accessing this generally really (mrc-5987)
      call("[[", call("dim", sym), as.numeric(dim))
    }
  } else if (is.recursive(expr)) {
    expr[-1] <- lapply(expr[-1], constraint_deparse_rewrite, arrays)
    expr
  } else {
    expr
  }
}
