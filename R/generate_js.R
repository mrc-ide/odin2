generate_js_system <- function(dat) {
  dat <- generate_prepare(dat)

  body <- collector()
  body$add(sprintf("const %s = {", dat$class))
  parts <- generate_js_parts()
  for (nm in names(parts)) {
    body$add(sprintf("  %s", parts[[nm]](dat)))
  }
  body$add("};")
  body$get()
}

generate_js_parts <- function() {
  list(
    build_params = generate_js_system_build_params,
    build_internal = generate_js_system_build_internal,
    initial = generate_js_system_initial,
    update = generate_js_system_update,
    rhs = generate_js_system_rhs,
    output = generate_js_system_output
  )
}

# Add imports at start of all args in js generator
js_generator_method <- function(name, args, body) {
  args <- c("imports", args)
  js_method(name, args, body)
}


generate_js_system_packing <- function(name, dat) {
  packing <- dat$storage$packing[[name]]
  arrays <- dat$storage$arrays
  dim_name <- arrays$alias[match(packing$name, arrays$name)]
  els <- ifelse(packing$rank == 0,
                sprintf('["%s", []]', packing$name),
                sprintf('["%s", dim.%s.dim]', packing$name, dim_name))
  ## trailing comma if needed
  els[-length(els)] <- sprintf("%s,", els[-length(els)])
  shape <- c("const shape = new Map([",
    sprintf("  %s", els),
    "]);")
  c(shape,
    "odin.packing = new imports.Packer({ shape });")
}

generate_js_system_build_params <- function(dat) {
  options <- list(shared_exists = FALSE)
  args <- c("params")
  body <- collector()
  body$add("const dim = imports.array.getDimObj();")
  body$add("const odin = {};")
  eqs <- get_phase_equations("build_shared", dat)
  for (eq in eqs) {
    body$add(generate_js_assignment(eq, "state", dat, options))
  }
  body$add(generate_js_system_packing("state", dat))

  is_dim <- vlapply(eqs, function(x) identical(x$special, "dim"))
  nms_dim <- vcapply(eqs[is_dim], function(x) x$lhs$name)
  nms_param <- setdiff(dat$phases$build_shared$equations, nms_dim)
  for (nm in nms_param) {
    body$add(sprintf("params.%s = %s;", nm, nm))
  }

  body$add("return { ...params, dim, odin };")

  js_generator_method("buildParams", args, body$get())
}



generate_js_system_build_internal <- function(dat) {
  args <- c("params")
  if (length(dat$storage$contents$internal) == 0) {
    body <- "return {};"
  } else {
    nms <- dat$storage$contents$internal
    size <- vcapply(nms, function(nm) {
      generate_js_sexp(call("OdinLength", nm), dat$sexp_data)
    })
    body <- c(
      "return {",
      sprintf("  %s: Array(%s).fill(0),", nms, size),
      "};"
    )
  }
  js_generator_method("buildInternal", args, body)
}



generate_js_system_initial <- function(dat) {
  args <- c("time, params, internal, state")
  body <- collector()
  eqs <- c(get_phase_equations("initial", dat),
           dat$phases$initial$variables)
  for (eq in eqs) {
    body$add(generate_js_assignment(eq, "state", dat))
  }
  js_generator_method("initial", args, body$get())
}



generate_js_unpack <- function(names, packing, sexp_data, from = "state") {
  if (length(names) == 0) {
    return(NULL)
  }
  i <- match(names, packing$name)
  is_scalar <- lengths(packing$dims[i]) == 0
  is_array <- !is_scalar
  starting <- c(
    sprintf(
      "const unpacked = params.odin.packing.unpackArrayNoCopy(%s);", from
      )
  )
  ret <- character(length(names))
  ret[is_scalar] <- sprintf("const %s = unpacked.get('%s');",
                            names[is_scalar],
                            names[is_scalar])
  ret[is_array] <- sprintf("const %s = unpacked.get('%s');",
                           names[is_array],
                           names[is_array])
  c(starting, ret)
}

generate_js_system_update <- function(dat) {
  ## dust has this comment and code but I think I will generate out
  ## update with empty body every time so that we do allow for mixed
  ## models

  ## I think this is not quite the right condition, because we do want
  ## this with a mixed model.
  # if (dat$time == "continuous") {
  #   return(NULL)
  # }

  args <- c("time", "dt", "state", "params", "internal", "stateNext", "random")
  body <- collector()
  unpack <- intersect(dat$variables, dat$phases$update$unpack)
  body$add(
    generate_js_unpack(unpack, dat$storage$packing$state, dat$sexp_data))
  eqs <- c(get_phase_equations("update", dat),
           dat$phases$update$variables)
  for (eq in eqs) {
    body$add(generate_js_assignment(eq, "stateNext", dat))
  }

  js_generator_method("update", args, body$get())
}



generate_js_system_rhs <- function(dat) {
  if (dat$time == "discrete") {
    return(NULL)
  }

  args <- c("params", "internal", "time", "state", "stateDeriv")

  body <- collector()

  if (any(dat$delays$in_rhs)) {
    stop("delays not supported in the js version")
  }

  unpack <- intersect(dat$variables, dat$phases$deriv$unpack)
  body$add(
    generate_js_unpack(unpack, dat$storage$packing$state, dat$sexp_data))
  eqs <- c(get_phase_equations("deriv", dat),
           dat$phases$deriv$variables)
  for (eq in eqs) {
    body$add(generate_js_assignment(eq, "stateDeriv", dat))
  }

  js_generator_method("rhs", args, body$get())
}


generate_js_system_output <- function(dat) {
  if (length(dat$output) == 0) {
    return(NULL)
  }
  args <- c("params", "internal", "time", "state")
  body <- collector()

  if (any(dat$delays$in_output)) {
    stop("delays not supported in the js version")
  }

  unpack <- intersect(dat$variables, dat$phases$output$unpack)
  body$add(
    generate_js_unpack(unpack, dat$storage$packing$state, dat$sexp_data))
  eqs <- get_phase_equations("output", dat)
  for (eq in eqs) {
    body$add(generate_js_assignment(eq, "state", dat))
  }

  packing <- dat$storage$packing$state
  for (el in dat$output) {
    src <- generate_js_sexp(el, dat$sexp_data)
    offset <- generate_js_sexp(call("OdinOffset", "state", el),
                                 dat$sexp_data)
    is_array <- el %in% dat$storage$arrays$name
    if (is_array) {
      body$add(sprintf(
        "imports.array.assignArrayToSliceOfArray(state, %s, %s);",
        offset, src))
    } else {
      body$add(sprintf("state[%s] = %s;", offset, src))
    }
  }

  js_generator_method("output", args, body$get())
}






generate_js_assignment <- function(eq, name_state, dat, options = list()) {
  if (eq$rhs$type == "parameter") {
    name <- eq$lhs$name
    type <- dat$parameters$type[dat$parameters$name == name]
    type_to_read <- list(
      real_type = "readReal",
      int = "readInt",
      size = "readSize",
      bool = "readBool"
    )
    read <- type_to_read[[type]]
    is_array <- name %in% dat$storage$arrays$name
    lhs <- generate_js_lhs(eq$lhs, dat, name_state, options)

    if (is_array) {
      i <- match(name, dat$storage$arrays$name)
      dim_name <- dat$storage$arrays$alias[i]
      if (isFALSE(options$shared_exists)) {
        dim <- sprintf("dim.%s", dim_name)
      } else {
        dim <- sprintf("params.dim.%s", dim_name)
      }
      rhs <- sprintf(
        'imports.io.%sArray(params, "%s", %s)',
        read, name, dim)
    } else {
      if (isFALSE(options$shared_exists)) {
        if (is.null(eq$rhs$args$default)) {
          rhs <- sprintf('imports.io.%s(params, "%s")', read, eq$lhs$name)
        } else {
          default <- generate_js_sexp(
            eq$rhs$args$default, dat$sexp_data, options)
          rhs <- sprintf('imports.io.%s(params, "%s", %s)',
                         read, eq$lhs$name, default)
        }
      } else {
        rhs <- sprintf('imports.io.%s(params, "%s", params.%s)',
                       read, eq$lhs$name, name)
      }
    }

    res <- sprintf("%s = %s;", lhs, rhs)

    for (constraint in c("min", "max")) {
      if (!is.null(eq$rhs$args[[constraint]])) {
        what <- if (is_array) "Array" else "Scalar"
        func <- if (constraint == "min") "Min" else "Max"
        constraint_value <- generate_js_sexp(eq$rhs$args[[constraint]],
                                             dat$sexp_data, options)
        dest <- sprintf("params.%s", name)
        res <- c(res,
                 sprintf('imports.io.check%s%s(%s, %s, "%s");',
                         func, what, dest, constraint_value, name))
      }
    }
  } else if (identical(eq$special, "dim")) {
    i <- match(eq$lhs$name_data, dat$storage$arrays$name)
    if (dat$storage$arrays$name[i] != dat$storage$arrays$alias[i]) {
      return(NULL)
    }
    rank <- dat$storage$arrays$rank[[i]]
    if (eq$rhs$is_user_sized) {
      stop("user sized dimensions not supported in js")
      # res <- sprintf(
      #   "dim.%s = imports.io.readDims(params, '%s', %d)",
      #   eq$lhs$name_data, eq$lhs$name_data, rank)
    } else {
      dims <- vcapply(dat$storage$arrays$dims[[i]], generate_js_sexp,
                      dat$sexp_data, options)
      dims_str <- paste(dims, collapse = ", ")
      res <- sprintf("dim.%s = [%s];", eq$lhs$name_data, dims_str)
    }
  } else if (identical(eq$rhs$type, "interpolate")) {
    name <- eq$lhs$name
    rhs <- generate_js_sexp(eq$rhs$expr, dat$sexp_data, options)
    if (isFALSE(options$shared_exists)) {
      dest <- name
      res <- sprintf("const %s = %s;", dest, rhs)
    } else {
      dest <- sprintf("params.%s", name)
      res <- sprintf("%s = %s;", dest, rhs)
    }
  } else if (identical(eq$rhs$type, "delay")) {
    stop("Delays are not support in the js version")
  } else {
    lhs <- generate_js_lhs(eq$lhs, dat, name_state, options)
    rhs <- generate_js_sexp(eq$rhs$expr, dat$sexp_data, options)

    res <- sprintf("%s = %s;", lhs, rhs)
    is_array <- !is.null(eq$lhs$array)
    if (is_array) {
      depends <- find_dependencies(eq$rhs$expr)$variables
      res <- generate_js_array_loops(
        res, depends, eq$lhs$array, dat$sexp_data, options)
    }
  }

  res
}

generate_js_array_loops <- function(inner, depends, array, sexp_data, options) {
  res <- inner
  for (idx in rev(array)) {
    if (idx$type == "range") {
      from <- generate_js_sexp(idx$from, sexp_data, options)
      to <- generate_js_sexp(idx$to, sexp_data, options)
      size_fns <- c("length", "dim", "OdinDim", "OdinLength")
      if (!rlang::is_call(idx$to, size_fns) && !is.numeric(idx$to)) {
        to <- sprintf("Number(%s)", to)
      }
      res <- c(sprintf("for (let %s = %s; %s <= %s; %s++) {",
                       idx$name, from, idx$name, to, idx$name),
               res,
               "}")
    } else if (idx$name %in% depends) {
      at <- generate_js_sexp(idx$at, sexp_data, options)
      res <- c("{",
               sprintf("const %s = %s;", idx$name, at),
               res,
               "}")
    }
  }
  if (length(res) > 1) {
    i_start <- c(FALSE, grepl("^(for|\\{)", res[-length(res)]))
    i_end <- grepl("^}", res)
    indent <- strrep("  ", cumsum(i_start - i_end))
    res <- paste0(indent, res)
  }

  res
}

generate_js_lhs <- function(lhs, dat, name_state, options) {
  name <- lhs$name
  is_array <- !is.null(lhs$array)
  if (is_array) {
    idx <- flatten_index(
      lapply(lhs$array, function(x) {
        if (x$type == "range") as.name(x$name) else x$at
      }),
      name)
  } else {
    idx <- NULL
  }

  location <- dat$storage$location[[name]]
  if (location == "stack") {
    if (is_array) {
      stop("We don't have stack-allocated arrays") # nocov
    }
    sprintf("const %s", name)
  } else if (location %in% c("shared", "internal")) {
    if (is_array) {
      stopifnot(!(name %in% dat$parameters$name))
      sprintf("%s[%s]",
              generate_js_sexp(name, dat$sexp_data, options),
              generate_js_sexp(idx, dat$sexp_data, options))
    } else if (location == "shared" && isFALSE(options$shared_exists)) {
      sprintf("const %s",
              generate_js_sexp(name, dat$sexp_data, options))
    } else {
      generate_js_sexp(name, dat$sexp_data, options)
    }
  } else if (location == "state") {
    offset <- call("OdinOffset", "state", name)
    if (is_array) {
      offset <- expr_plus(idx, offset)
    }
    sprintf("%s[%s]",
            name_state,
            generate_js_sexp(offset, dat$sexp_data, options))
  } else if (location == "adjoint") {
    stop("Adjoint location not support in js")
  } else {
    stop(sprintf("Unsupported location '%s'", location))
  }
}


generate_js_sexp_reduce <- function(expr, dat, options) {
  fn <- expr$fn
  target <- expr$what
  target_str <- generate_js_sexp(target, dat, options)
  index <- expr$index
  dim <- paste0(
    if (isFALSE(options$shared_exists)) "dim." else "params.dim.",
    dat$arrays$alias[match(target, dat$arrays$name)])
  stopifnot(fn %in% c("sum", "prod", "min", "max"))
  if (is.null(index)) {
    sprintf("imports.array.%sAll(%s)", fn, target_str)
  } else {
    index_len <- length(index)
    stopifnot(index_len <= 4)

    index_str <- paste(vcapply(index, function(el) {
      if (el$type == "single") {
        from <- expr_minus(el$at, 1)
        to <- from
      } else {
        from <- expr_minus(el$from, 1)
        to <- expr_minus(el$to, 1)
      }
      sprintf("[%s, %s]",
              generate_js_sexp(from, dat, options),
              generate_js_sexp(to, dat, options))
    }), collapse = ", ")

    sprintf("imports.array.%s%d(%s, %s, %s)",
            fn, index_len, target_str, dim, index_str)
  }
}

generate_js_array_access <- function(expr, dat, options) {
  target <- generate_js_sexp(expr[[2]], dat, options)
  name <- as.character(expr[[2]])
  options$array_access <- TRUE
  idx <- generate_js_sexp(flatten_index(expr[-(1:2)], name), dat, options)
  sprintf("%s[%s]", target, idx)
}

generate_js_sexp <- function(expr, dat, options = list()) {
  if (is.recursive(expr)) {
    is_stochastic_call <- rlang::is_call(expr[[1]], "OdinStochasticCall")
    if (is_stochastic_call) {
      if (isTRUE(options$stochastic_expectation)) {
        return(generate_js_sexp(expr[[1]]$mean, dat, options))
      }
      fn <- expr[[1]]$sample
    } else {
      fn <- as.character(expr[[1]])
    }

    ## There's a group here where we don't want to evaluate the
    ## arguments, because the interpretation of some values will be
    ## different to odin's normal rewrite semantics.
    if (fn == "[") {
      return(generate_js_array_access(expr, dat, options))
    } else if (fn == "OdinDim") {
      dim <- if (isFALSE(options$shared_exists)) "dim." else "params.dim."
      name <- dat$arrays$alias[match(expr[[2]], dat$arrays$name)]
      return(sprintf("%s%s.dim[%d]", dim, name, expr[[3]] - 1))
    } else if (fn == "OdinLength") {
      dim <- if (isFALSE(options$shared_exists)) "dim." else "params.dim."
      name <- dat$arrays$alias[match(expr[[2]], dat$arrays$name)]
      return(sprintf("%s%s.size", dim, name))
    } else if (fn == "OdinMult") {
      dim <- if (isFALSE(options$shared_exists)) "dim." else "params.dim."
      name <- dat$arrays$alias[match(expr[[2]], dat$arrays$name)]
      return(sprintf("%s%s.mult[%d]", dim, name, expr[[3]] - 1))
    } else if (fn == "OdinOffset") {
      where <- expr[[2]]
      what <- expr[[3]]
      packing <- dat$packing[[where]]
      i <- match(what, packing$name)
      if (is.na(packing$offset[[i]])) {
        shared <- if (isFALSE(options$shared_exists)) "" else "params."
        return(sprintf("%sodin.packing.idx.%s.start", shared, what))
      } else {
        return(as.character(packing$offset[[i]]))
      }
    } else if (fn == "length") {
      return(generate_js_sexp(call("OdinLength", as.character(expr[[2]])),
                                dat, options))
    } else if (fn %in% c("nrow", "ncol")) {
      return(generate_js_sexp(
        call("OdinDim", as.character(expr[[2]]), if (fn == "nrow") 1 else 2),
        dat, options))
    } else if (fn == "OdinReduce") {
      return(generate_js_sexp_reduce(expr, dat, options))
    } else if (fn == "OdinInterpolateAlloc") {
      mode <- expr$mode
      rank <- expr$rank
      substr(mode, 1, 1) <- toupper(substr(mode, 1, 1))
      time_var <- generate_js_sexp(expr$time, dat, options)
      value_var <- generate_js_sexp(expr$value, dat, options)
      stopifnot(mode == "Constant")
      if (rank == 0) {
        return(sprintf(
          'new imports.interpolate.Interpolate%s(%s, %s, "%s", "%s")',
          mode, time_var, value_var, expr$time, expr$value
        ))
      } else {
        dim <- if (isFALSE(options$shared_exists)) "dim." else "params.dim."
        dim_var <- sprintf("%s%s", dim, expr$dim)
        return(sprintf(
          'new imports.interpolate.Interpolate%sArray(%s, %s, %s, "%s", "%s")',
          mode, time_var, value_var, dim_var, expr$time, expr$value
        ))
      }
    } else if (fn == "OdinInterpolateEval") {
      src <- generate_js_sexp(expr[[2]], dat, options)
      target <- expr[[3]]
      return(sprintf("%s.eval(time);", src))
    }

    ## Below here is much simpler, really.
    args_value <- as.list(expr[-1])
    args_str <- vcapply(args_value, generate_js_sexp, dat, options)
    n <- length(args_value)

    binary_compare <- c("==", "!=", "<", ">", "<=", ">=")
    binary_inplace <- c("+", "-", "*", "/", "&&", "||", binary_compare)

    if (fn == "(") {
      ret <- sprintf("(%s)", args_str[[1]])
    } else if (n == 1 && fn == "-") {
      ret <- sprintf("-%s", args_str[[1]])
    } else if (n == 1 && fn == "+") {
      ret <- args_str[[1]]
    } else if (n == 2 && fn %in% binary_inplace) {
      js_fn <- fn
      if (fn %in% c("==", "!=")) {
        ## js equality and non equality operators do type conversion by
        ## default but we don't want this behaviour so convert to strict
        ## versions of those operators
        js_fn <- if (fn == "==") "===" else "!=="
      }
      ret <- sprintf("%s %s %s", args_str[[1]], js_fn, args_str[[2]])
    } else if (fn == "%%") {
      ret <- sprintf("%s %% %s", args_str[[1]], args_str[[2]])
    } else if (fn == "%/%") {
      ret <- sprintf("Math.floor(%s / %s)", args_str[[1]], args_str[[2]])
    } else if (fn %in% c("min", "max")) {
      ret <- sprintf("Math.%s(%s)", fn, paste(args_str, collapse = ", "))
      if (isTRUE(options$array_access)) {
        ## If we are within a `[` expression then we're almost certainly
        ## wanting an integer out
        ret <- sprintf("Math.round(%s)", ret)
      }
    } else if (fn %in% names(FUNCTIONS_JS_MATH)) {
      ret <- sprintf("%s(%s)",
                     FUNCTIONS_JS_MATH[[fn]],
                     paste(args_str, collapse = ", "))
    } else if (fn == "as.numeric") {
      ret <- sprintf("Number(%s)", args_str[[1]])
    } else if (fn == "as.integer") {
      ret <- sprintf("Math.round(%s)", args_str[[1]])
    } else if (fn == "if") {
      ## NOTE: The ternary operator has very low precendence, so we
      ## will agressively parenthesise it.  This is strictly not
      ## needed when this expression is the only element of `expr` but
      ## that's hard to detect so we'll tolerate a few additional
      ## parens for now.
      ret <- sprintf("(%s ? %s : %s)",
                     args_str[[1L]], args_str[[2L]], args_str[[3L]])
    } else if (is_stochastic_call) {
      ## Not all distributions are in random in dust2-js but for now
      ## just letting it error
      stopifnot(fn %in% c("binomial", "uniform", "normal", "poisson"))
      ret <- sprintf("random.%s(%s)",
                     fn, paste(args_str, collapse = ", "))
    } else if (fn == "as.logical") {
      ret <- sprintf("Boolean(%s)", args_str[[1]])
    } else {
      ## TODO: we should catch this during parse; erroring here is a
      ## bug as we don't offer context.
      cli::cli_abort("Unhandled function '{fn}'",
                     class = "odin_bug")
    }
  } else {
    if (is.symbol(expr) || is.character(expr)) {
      name <- as.character(expr)
      if (name %in% c("time", "dt", INDEX)) {
        ret <- name
      } else if (name == "pi") {
        ret <- "Math.PI"
      } else {
        location <- dat$location[[name]]
        shared_exists <- !isFALSE(options$shared_exists)
        if (location %in% c("state", "stack", "adjoint",
                            if (!shared_exists) "shared")) {
          ret <- name
        } else { # params, internal, data
          js_location <- if (location == "shared") "params" else location
          ret <- sprintf("%s.%s", js_location, name)
        }
      }
    } else if (is.numeric(expr)) {
      if (expr == Inf) {
        ret <- "Infinity"
      } else if (expr %% 1 == 0) {
        ret <- format(expr)
      } else {
        ret <- deparse(expr, control = "digits17")
      }
    } else if (is.logical(expr)) {
      ret <- tolower(expr)
    } else {
      cli::cli_abort("Unhandled data type while generating expression",
                     class = "odin_bug")
    }
  }
  ret
}
