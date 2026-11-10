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
  allocated <- character()
  for (eq in eqs) {
    name <- eq$lhs$name
    if (
        name %in% dat$storage$arrays$name
        && !(name %in% allocated)
        && !identical(eq$special, "parameter")
      ) {
      size <- generate_js_sexp(
        call("OdinLength", name),
        dat$sexp_data, options)
      body$add(sprintf("const %s = Array(%s).fill(0);", name, size))
      allocated <- c(allocated, name)
    }
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
      "const unpacked = params.odin.packing.unpackArray(%s);", from
      )
  )
  ret <- character(length(names))
  ret[is_scalar] <- sprintf("const %s = unpacked.get('%s');",
                            names[is_scalar],
                            names[is_scalar])
  ret[is_array] <- sprintf("const %s = unpacked.get('%s').data;",
                           names[is_array],
                           names[is_array])
  c(starting, ret)
}


generate_js_system_update <- function(dat) {
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

  # don't include update function if body is empty because the runner
  # either solves with dt or solver based on if the update function is
  # defined
  if (body$length() == 0) {
    NULL
  } else {
    js_generator_method("update", args, body$get())
  }
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
    if (isFALSE(options$shared_exists)) {
      dest <- name
    } else {
      dest <- sprintf("params.%s", name)
    }

    if (is_array) {
      i <- match(name, dat$storage$arrays$name)
      dim_name <- dat$storage$arrays$alias[i]
      if (isFALSE(options$shared_exists)) {
        dim <- sprintf("dim.%s", dim_name)
      } else {
        dim <- sprintf("params.dim.%s", dim_name)
      }
      dest <- generate_js_sexp(name, dat$sexp_data, options)
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
