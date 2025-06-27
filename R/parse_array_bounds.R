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
  ## that we will have found; it's likely that we will have n <= EXPR
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
  constraints <- c(
    unlist0(lapply(check, parse_array_bounds_extract_constraint)),
    parse_delays_extract_constraint(dat))

  ## Convert this list-of-lists into a nicer data frame:
  constraints <- data.frame(
    type = vcapply(constraints, "[[", "type"),
    name = vcapply(constraints, "[[", "name"),
    expr = I(lapply(constraints, "[[", "expr")),
    dimension = viapply(constraints, "[[", "dimension"),
    at = I(lapply(constraints, "[[", "at")),
    mode = vcapply(constraints, "[[", "mode"),
    src = viapply(constraints, "[[", "src"))

  ## Sort and deduplicate the worst of the repetition:
  constraints <- constraints[order(constraints$name, constraints$type), ]
  at_hash <- vcapply(constraints$at, rlang::hash)
  id <- paste(constraints$name, constraints$mode, at_hash, sep = ":")
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
          if (v != "to") {
            ret$add(constraint(
              "access:write", name, expr, i, idx[[v]], "min", eq$src$index))
          }
          if (v != "from") {
            ret$add(constraint(
              "access:write", name, expr, i, idx[[v]], "max", eq$src$index))
          }
        }
      }
    }
  } else if (rlang::is_call(eq$rhs$expr, "OdinInterpolateEval")) {
    name <- eq$lhs$name
    expr <- eq$src$value
    rank <- eq$rhs$info$rank
    time <- eq$rhs$info$time
    value <- eq$rhs$info$value

    ## We need to add:
    ## * 1 constraint for length(time) == dim(value, rank)
    ## * n constraints for dim(name, i) == dim(value, i) over all dimensions
    len_t <- call("OdinLength", time)
    ret$add(constraint(
      "interpolate:time", value, time, rank + 1L, len_t, "exact", eq$src$index))
    for (i in seq_len(rank)) {
      len_i <- call("OdinDim", value, i)
      ret$add(constraint(
        "interpolate:value", name, expr, i, len_i, "exact", eq$src$index))
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
          ret$add(constraint(
            "access:read", name, expr, i, at[[i]], "min", eq$src$index))
          ret$add(constraint(
            "access:read", name, expr, i, at[[i]], "max", eq$src$index))
        } else if (length(uses) == 1) {
          idx <- eq$lhs$array[[match(uses, INDEX)]]
          for (v in c("from", "to", "at")) {
            if (!is.null(idx[[v]])) {
              at_i <- substitute_(at[[i]], set_names(idx[v], uses))
              if (v != "to") {
                ret$add(constraint(
                  "access:read", name, expr, i, at_i, "min", eq$src$index))
              }
              if (v != "from") {
                ret$add(constraint(
                  "access:read", name, expr, i, at_i, "max", eq$src$index))
              }
            }
          }
        } else {
          warn_unhandled_analysis(expr, uses)
        }
      }
    } else if (rlang::is_call(expr, "OdinReduce")) {
      name <- expr$what
      idx <- expr$index
      expr_reduce <- expr$expr[[2]]
      for (i in seq_along(idx)) {
        for (v in c("from", "to", "at")) {
          at <- idx[[i]][[v]]
          if (!is.null(at)) {
            uses <- intersect(all.vars(at), INDEX)
            if (length(uses) == 0) {
              ret$add(constraint(
                "access:read", name, expr_reduce, i, at, "max", eq$src$index))
            } else if (length(uses) == 1) {
              idx_lhs <- eq$lhs$array[[match(uses, INDEX)]]
              for (v_lhs in c("from", "to", "at")) {
                v_lhs <- if (!is.null(idx_lhs$to)) "to" else "at"
                at_i <- substitute_(at, set_names(idx_lhs[v_lhs], uses))
              }
              ret$add(constraint(
                "access:read", name, expr_reduce, i, at_i, "max", eq$src$index))
            } else {
              warn_unhandled_analysis(expr$expr, uses)
            }
          }
        }
      }
    } else if (is.recursive(expr)) {
      lapply(expr[-1], extract)
    }
  }

  extract(eq$rhs$expr)
  ret$get()
}


constraint <- function(type, name, expr, dimension, at, mode, src) {
  ## We'll enforce elsewhere that all arrays have length 1 or more,
  ## not sure where though!
  if (is.numeric(at) && at == 1) {
    return(NULL)
  }
  list(
    list(
      type = type,
      name = name,
      expr = expr,
      dimension = dimension,
      at = at,
      mode = mode,
      src = src))
}


## At this point we only have the constraints for a single array
constraint_triage <- function(d, arrays, equations, variables, src, call) {
  throw_bad_access <- function(i) {
    src_index <- d$src[[i]]
    rank <- length(arrays[[name]])
    dim <- d$dimension[[i]]
    at_value <- constraint_deparse(at[[i]], arrays)
    size_value <- constraint_deparse(size[[i]], arrays)
    expr <- src[[src_index]]

    is_interpolate <- startsWith(d$type[[i]], "interpolate:")

    if (is_interpolate) {
      msg <- "Incompatible size arrays in arguments to 'interpolate()'"
      if (d$type[[i]] == "interpolate:time") {
        time <- d$expr[[1]]
        value <- name
        time_length <- at_value
        value_length <- size_value
        if (rank == 1) {
          odin_parse_error(
            c(msg,
              x = "'{value}' must have the same length as '{time}'",
              i = "'{value}' has length: {value_length}",
              i = "'{time}' has length: {time_length}"),
            "E3002", expr, call)
        } else {
          odin_parse_error(
            c(msg,
              x = paste("The last dimension of '{value}' must be the same",
                        "as the length of '{time}'"),
              i = "Dimension {dim} of '{value}' has size: {value_length}",
              i = "'{time}' has length: {time_length}"),
            "E3002", expr, call)
        }
      } else {
        info <- equations[[name]]$rhs$info
        target <- name
        value <- info$value
        target_length <- size_value
        value_length <- at_value
        odin_parse_error(
          c(msg,
            x = paste("The first {rank} dimensions of '{value}' must be",
                      "the same as the dimensions of '{target}', but",
                      "they differ at dimension {dim}"),
            i = "Dimension {dim} of '{target}' has size: {target_length}",
            i = "Dimension {dim} of '{value} has size: {value_length}"),
          "E3002", expr, call)
      }
    } else {
      expr_str <- deparse1(d$expr[[i]])
      access <- if (d$type[[i]] == "access:read") "read" else "write"
      if (rank == 1) {
        msg <- "Out of range {access} of '{name}' in '{expr_str}'"
        if (d$mode[[i]] == "min") {
          hint_size <- "Attempting to read before the start of '{name}'"
        } else {
          hint_size <- "'{name}' has capacity: {size_value}"
        }
      } else {
        msg <-
          "Out of range {access} of '{name}' in dimension {dim} of '{expr_str}'"
        if (d$mode[[i]] == "min") {
          hint_size <-
            "Attempting to read before the start of dimension {dim} of '{name}'"
        } else {
          hint_size <- "Dimension {dim} of '{name}' has capacity: {size_value}"
        }
      }
      hint_access <- "Trying to {access} element: {at_value}"
      odin_parse_error(
        c(msg,
          i = hint_size,
          x = hint_access),
        "E3001", expr, call)
    }
  }

  name <- d$name[[1]]
  rank <- length(arrays[[name]])
  at <- lapply(d$at, constraint_simplify_expr, arrays, equations, variables)
  size <- arrays[[name]][d$dimension]
  result <- Map(constraint_solve, at, size, d$mode)

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


constraint_solve <- function(at, size, mode) {
  ## We can compute the validity here immediately:
  if (is.numeric(at) && (is.numeric(size) || mode == "min")) {
    if (mode == "exact") {
      return(list(valid = at == size))
    } else if (mode == "max") {
      return(list(valid = at <= size))
    } else { # mode == "min"
      return(list(valid = at >= 1))
    }
  }

  ## This is common enough that we can also determine that we're ok:
  if (identical(at, size)) {
    return(list(valid = TRUE))
  }
  maths <- monty::monty_differentiation()$maths

  if (mode == "min") {
    expr <- maths$minus(at, 1)
  } else {
    at <- maths$plus_fold(lapply(expr_to_sum_of_parts(at), maths$uminus))
    expr <- expr_factorise(maths$plus(size, at))
  }

  if (is.numeric(expr)) {
    return(list(valid = if (mode == "exact") expr == 0 else expr >= 0))
  }

  list(valid = NA, constraint = constraint_tidy(expr, mode))
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
  constraints <- constraints[order(constraints$name), ]
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


constraint_tidy <- function(expr, mode) {
  op <- if (mode == "exact") "==" else ">="
  if ("OdinParameter" %in% all.names(expr)) {
    maths <- monty::monty_differentiation()$maths
    parts <- expr_to_sum_of_parts(expr)
    i <- vlapply(parts, rlang::is_call, "OdinParameter")
    call(op,
         maths$plus_fold(parts[i]),
         maths$plus_fold(lapply(parts[!i], maths$uminus)))
  } else {
    call(op, expr, 0)
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


parse_delays_extract_constraint <- function(dat) {
  delays <- dat$delays
  if (is.null(delays)) {
    return(NULL)
  }
  arrays <- dat$storage$arrays

  ret <- collector(list())

  for (i in seq_len(nrow(delays))) {
    name <- delays$name[[i]]
    what <- delays$value[[i]]$what
    if (name %in% arrays$name) {
      idx_name <- match(name, arrays$name)
      idx_what <- match(what, arrays$name)
      if (arrays$alias[[idx_name]] != arrays$alias[[idx_what]]) {
        eq <- dat$equations[[name]]
        rank <- arrays$rank[[idx_name]]
        dims_name <- arrays$dims[[idx_name]]
        dims_what <- arrays$dims[[idx_what]]
        expr <- eq$src$value
        for (j in seq_len(rank)) {
          ret$add(constraint(
            "access:write", name, expr, j, call("OdinDim", what, j), "max",
            eq$src$index))
          ret$add(constraint(
            "access:read", what, expr, j, call("OdinDim", name, j), "max",
            eq$src$index))
        }
      }
    }
  }

  ret$get()
}
