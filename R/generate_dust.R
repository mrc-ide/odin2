generate_dust_system <- function(dat) {
  dat <- generate_prepare(dat)

  body <- collector()
  body$add("#include <dust2/common.hpp>")
  body$add(generate_dust_system_attributes(dat))
  body$add(sprintf("class %s {", dat$class))
  body$add("public:")
  body$add(sprintf("  %s() = delete;", dat$class))
  body$add("  using real_type = double;")
  body$add("  using rng_state_type = monty::random::generator<real_type>;")
  body$add(sprintf("  %s", generate_dust_system_shared_state(dat)))
  body$add(sprintf("  %s", generate_dust_system_internal_state(dat)))
  body$add(sprintf("  %s", generate_dust_system_data_type(dat)))
  body$add(sprintf("  %s", generate_dust_system_packing_state(dat)))
  body$add(sprintf("  %s", generate_dust_system_packing_gradient(dat)))
  body$add(sprintf("  %s", generate_dust_system_build_shared(dat)))
  body$add(sprintf("  %s", generate_dust_system_build_internal(dat)))
  body$add(sprintf("  %s", generate_dust_system_build_data(dat)))
  body$add(sprintf("  %s", generate_dust_system_update_shared(dat)))
  body$add(sprintf("  %s", generate_dust_system_update_internal(dat)))
  body$add(sprintf("  %s", generate_dust_system_initial(dat)))
  body$add(sprintf("  %s", generate_dust_system_update(dat)))
  body$add(sprintf("  %s", generate_dust_system_rhs(dat)))
  body$add(sprintf("  %s", generate_dust_system_zero_every(dat)))
  body$add(sprintf("  %s", generate_dust_system_compare_data(dat)))
  body$add(sprintf("  %s", generate_dust_system_adjoint(dat)))
  body$add("};")
  body$get()
}


generate_prepare <- function(dat) {
  rank <- set_names(dat$storage$arrays$rank, dat$storage$arrays$name)
  dat$sexp_data <- generate_dust_dat(dat$storage$location,
                                     dat$storage$packing,
                                     rank)
  dat
}


generate_dust_system_attributes <- function(dat) {
  if (length(dat$phases$compare) == 0) {
    has_compare <- NULL
  } else {
    has_compare <- "// [[dust2::has_compare()]]"
  }
  if (is.null(dat$adjoint)) {
    has_adjoint <- NULL
  } else {
    has_adjoint <- "// [[dust2::has_adjoint()]]"
  }
  c(sprintf("// [[dust2::class(%s)]]", dat$class),
    sprintf("// [[dust2::time_type(%s)]]", dat$time),
    has_compare,
    has_adjoint)
}


generate_dust_system_shared_state <- function(dat) {
  nms <- dat$storage$contents$shared
  type <- dat$storage$type[nms]
  is_array <- nms %in% dat$storage$arrays$name
  type[is_array] <- sprintf("std::vector<%s>", type[is_array])

  if (nrow(dat$storage$arrays) > 0) {
    dims <- c(
      "struct dim_type {",
      sprintf("  dust2::array::dimensions<%d> %s;",
              dat$storage$arrays$rank, dat$storage$arrays$name),
      "} dim;")
  } else {
    dims <- NULL
  }

  offset <- unlist0(lapply(names(dat$storage$packing), function(nm) {
    c("  struct {",
      sprintf("    size_t %s;", dat$storage$packing[[nm]]$name),
      sprintf("  } %s;", nm))
  }))
  offset <- c("struct offset_type {", offset, "} offset;")
  c("struct shared_state {",
    sprintf("  %s", dims),
    sprintf("  %s", offset),
    sprintf("  %s %s;", type, nms),
    "};")
}


generate_dust_system_internal_state <- function(dat) {
  if (length(dat$storage$contents$internal) == 0) {
    "struct internal_state {};"
  } else {
    nms <- dat$storage$contents$internal
    stopifnot(all(nms %in% dat$storage$arrays$name)) # just assert for now
    c("struct internal_state {",
      sprintf("  std::vector<%s> %s;", dat$storage$type[nms], nms),
      "};")
  }
}


generate_dust_system_data_type <- function(dat) {
  data <- dat$storage$contents$data
  if (length(data) == 0) {
    "using data_type = dust2::no_data;"
  } else {
    c("struct data_type {",
      sprintf("  real_type %s;", data),
      "};")
  }
}


generate_dust_system_packing_state <- function(dat) {
  generate_dust_system_packing("state", dat)
}


generate_dust_system_packing_gradient <- function(dat) {
  generate_dust_system_packing("gradient", dat)
}


generate_dust_system_packing <- function(name, dat) {
  packing <- dat$storage$packing[[name]]
  args <- c("const shared_state&" = "shared")
  fmt <- "std::vector<size_t>(shared.dim.%s.dim.begin(), shared.dim.%s.dim.end())"
  dims <- ifelse(packing$rank == 0, "{}",
                 sprintf(fmt, packing$name, packing$name))
  els <- sprintf('{"%s", %s}', packing$name, dims)
  ## trailing comma if needed
  els[-length(els)] <- sprintf("%s,", els[-length(els)])
  body <- c("return dust2::packing{",
            sprintf("  %s", els),
            "};")
  cpp_function("dust2::packing", sprintf("packing_%s", name), args, body,
               static = TRUE)
}


generate_dust_system_build_shared <- function(dat) {
  options <- list(shared_exists = FALSE)
  body <- collector()
  eqs <- get_phase_equations("build_shared", dat)
  allocated <- character()
  for (eq in eqs) {
    name <- eq$lhs$name
    if (name %in% dat$storage$arrays$name && !(name %in% allocated)) {
      i <- match(name, dat$storage$arrays$name)
      size <- generate_dust_sexp(
        call("OdinLength", name),
        dat$sexp_data, options)
      body$add(sprintf("std::vector<%s> %s(%s);",
                       dat$storage$type[[name]], name, size))
      allocated <- c(allocated, name)
    }
    body$add(generate_dust_assignment(eq, "state", dat, options))
  }
  is_dim <- vlapply(eqs, function(x) identical(x$special, "dim"))
  if (any(is_dim)) {
    nms_dim <- vcapply(eqs[is_dim], function(x) x$lhs$name)
    body$add(sprintf("const shared_state::dim_type dim{%s};",
                     paste(nms_dim, collapse = ", ")))
    nms_return <- c("dim", "offset",
                    setdiff(dat$phases$build_shared$equations, nms_dim))
  } else {
    nms_return <- c("offset", dat$phases$build_shared$equations)
  }

  offset <- unlist0(lapply(names(dat$storage$packing), function(nm) {
    value <- vcapply(dat$storage$packing[[nm]]$offset, generate_dust_sexp,
                     dat$sexp_data, options)
    sprintf("offset.%s.%s = %s;", nm, dat$storage$packing[[nm]]$name, value)
  }))

  body$add("shared_state::offset_type offset;")
  body$add(offset)
  body$add(sprintf("return shared_state{%s};",
                   paste(nms_return, collapse = ", ")))
  args <- c("cpp11::list" = "parameters")
  cpp_function("shared_state", "build_shared", args, body$get(), static = TRUE)
}


generate_dust_system_build_data <- function(dat) {
  data <- dat$storage$contents$data
  if (length(data) == 0) {
    return(NULL)
  }
  ## This is very simple for now, but later we can allow data to have
  ## types, lengths, etc.
  eqs <- dat$phases$create_data$equations
  body <- collector()
  body$add(sprintf('auto %s = dust2::r::read_real(data, "%s");', data, data))
  body$add(sprintf("return data_type{%s};", paste(data, collapse = ", ")))
  args <- c("cpp11::list" = "data",
            "const shared_state&" = "shared")
  cpp_function("data_type", "build_data", args, body$get(), static = TRUE)
}


generate_dust_system_build_internal <- function(dat) {
  args <- c("const shared_state&" = "shared")
  if (length(dat$storage$contents$internal) == 0) {
    body <- "return internal_state{};"
  } else {
    nms <- dat$storage$contents$internal
    type <- dat$storage$type[nms]
    size <- vcapply(nms, function(nm) {
      generate_dust_sexp(call("OdinLength", nm), dat$sexp_data)
    })
    body <- c(
      sprintf("std::vector<%s> %s(%s);", type, nms, size),
      sprintf("return internal_state{%s};", paste(nms, collapse = ", ")))
  }
  cpp_function("internal_state", "build_internal", args, body, static = TRUE)
}


generate_dust_system_update_shared <- function(dat) {
  body <- collector()
  eqs <- get_phase_equations("update_shared", dat)
  for (eq in eqs) {
    body$add(generate_dust_assignment(eq, "state", dat))
  }
  args <- c("cpp11::list" = "parameters", "shared_state&" = "shared")
  cpp_function("void", "update_shared", args, body$get(), static = TRUE)
}


generate_dust_system_update_internal <- function(dat) {
  args <- c("const shared_state&" = "shared", "internal_state&" = "internal")
  body <- character()
  cpp_function("void", "update_internal", args, body, static = TRUE)
}


generate_dust_system_initial <- function(dat) {
  if (dat$time == "continuous") {
    args <- c("real_type" = "time",
              "const shared_state&" = "shared",
              "internal_state&" = "internal",
              "rng_state_type&" = "rng_state",
              "real_type*" = "state")
  } else {
    args <- c("real_type" = "time",
              "const shared_state&" = "shared",
              "internal_state&" = "internal",
              "rng_state_type&" = "rng_state",
              "real_type*" = "state")
  }
  body <- collector()
  eqs <- c(get_phase_equations("initial", dat),
           dat$phases$initial$variables)
  for (eq in eqs) {
    body$add(generate_dust_assignment(eq, "state", dat))
  }
  cpp_function("void", "initial", args, body$get(), static = TRUE)
}


generate_dust_system_update <- function(dat) {
  ## I think this is not quite the right condition, because we do want
  ## this with a mixed model.
  if (dat$time == "continuous") {
    return(NULL)
  }
  args <- c("real_type" = "time",
            "real_type" = "dt",
            "const real_type*" = "state",
            "const shared_state&" = "shared",
            "internal_state&" = "internal",
            "rng_state_type&" = "rng_state",
            "real_type*" = "state_next")
  body <- collector()
  unpack <- intersect(dat$variables, dat$phases$update$unpack)
  body$add(
    generate_dust_unpack(unpack, dat$storage$packing$state, dat$sexp_data))
  eqs <- c(get_phase_equations("update", dat),
           dat$phases$update$variables)
  for (eq in eqs) {
    body$add(generate_dust_assignment(eq, "state_next", dat))
  }
  cpp_function("void", "update", args, body$get(), static = TRUE)
}


generate_dust_system_rhs <- function(dat) {
  if (dat$time == "discrete") {
    return(NULL)
  }
  args <- c("real_type" = "time",
            "const real_type*" = "state",
            "const shared_state&" = "shared",
            "internal_state&" = "internal",
            "real_type*" = "state_deriv")
  body <- collector()
  unpack <- intersect(dat$variables, dat$phases$deriv$unpack)
  body$add(
    generate_dust_unpack(unpack, dat$storage$packing$state, dat$sexp_data))
  eqs <- c(get_phase_equations("deriv", dat),
           dat$phases$deriv$variables)
  for (eq in eqs) {
    body$add(generate_dust_assignment(eq, "state_deriv", dat))
  }
  cpp_function("void", "rhs", args, body$get(), static = TRUE)
}


generate_dust_system_zero_every <- function(dat) {
  args <- c("const shared_state&" = "shared")
  if (is.null(dat$zero_every)) {
    body <- "return dust2::zero_every_type<real_type>();"
  } else {
    packing <- dat$storage$packing$state
    every <- vcapply(dat$zero_every, generate_dust_sexp, dat$sexp_data,
                     USE.NAMES = FALSE)
    i <- match(names(dat$zero_every), packing$name)
    is_array <- packing$rank[i] > 0
    nms <- sprintf("zero_every_%s", packing$name[i])
    nms[!is_array] <- sprintf(
      "{%s}",
      vcapply(packing$offset[i][!is_array], generate_dust_sexp, dat$sexp_data))
    if (any(is_array)) {
      create_vector <- function(name, offset, size) {
        end <- generate_dust_sexp(expr_plus(offset, size), dat$sexp_data)
        offset <- generate_dust_sexp(offset, dat$sexp_data)
        c(sprintf("std::vector<size_t> %s;", name),
          sprintf("for (size_t i = %s; i < %s; ++i) {", offset, end),
          sprintf("  %s.push_back(i);", name),
          "}")
      }
      create <- unlist0(Map(create_vector,
                            nms[is_array],
                            packing$offset[i][is_array],
                            packing$size[i][is_array]))
    } else {
      create <- NULL
    }
    str <- paste(sprintf("{%s, %s}", every, nms), collapse = ", ")
    body <- c(
      create,
      sprintf("return dust2::zero_every_type<real_type>{%s};", str))
  }
  cpp_function("auto", "zero_every", args, body, static = TRUE)
}


generate_dust_system_compare_data <- function(dat) {
  if (length(dat$phases$compare) == 0) {
    return(NULL)
  }
  args <- c("real_type" = "time",
            "const real_type*" = "state",
            "const data_type&" = "data",
            "const shared_state&" = "shared",
            "internal_state&" = "internal",
            "rng_state_type&" = "rng_state")
  body <- collector()
  unpack <- intersect(dat$variables, dat$phases$compare$unpack)
  body$add(
    generate_dust_unpack(unpack, dat$storage$packing$state, dat$sexp_data))
  ## TODO collision here in names with 'll'; we might need to prefix
  ## with compare_ perhaps?
  body$add("real_type ll = 0;")

  eqs <- get_phase_equations("compare", dat)
  for (eq in eqs) {
    body$add(generate_dust_assignment(eq, "state", dat))
  }

  ## Then the actual comparison:
  for (eq in dat$phases$compare$compare) {
    eq_args <- vcapply(eq$rhs$args, generate_dust_sexp, dat$sexp_data)

    ## TODO: when we refactor things, this probably belongs in the parse phase.
    ##
    ## This finds (recursively) all data used in the calculation, so
    ## that we can skip the calculation if any of this is NA.
    vars <- eq$rhs$depends$variables
    nms_eqs <- intersect(vars, names(dat$equations))
    vars <- union(
      vars,
      unlist0(lapply(dat$equations[names(dat$equations) %in% nms_eqs],
                     function(eq) eq$rhs$depends$variables_recursive)))
    nms_data <- intersect(vars, dat$data$name)

    check_data <- sprintf("!std::isnan(%s)",
                          vcapply(nms_data, generate_dust_sexp, dat$sexp_data))
    body$add(sprintf("if (%s) {", paste(check_data, collapse = " && ")))
    body$add(sprintf("  ll += monty::density::%s(%s, true);",
                     eq$rhs$density$cpp, paste(eq_args, collapse = ", ")))
    body$add("}")
  }

  body$add("return ll;")
  cpp_function("real_type", "compare_data", args, body$get(), static = TRUE)
}


generate_dust_system_adjoint <- function(dat) {
  if (is.null(dat$adjoint)) {
    return(NULL)
  }

  c(generate_dust_system_adjoint_update(dat),
    generate_dust_system_adjoint_compare_data(dat),
    generate_dust_system_adjoint_initial(dat))
}


generate_dust_system_adjoint_update <- function(dat) {
  args <- c("real_type" = "time",
            "real_type" = "dt",
            "const real_type*" = "state",
            "const real_type*" = "adjoint",
            "const shared_state&" = "shared",
            "internal_state&" = "internal",
            "real_type*" = "adjoint_next")
  body <- collector()

  unpack <- intersect(dat$variables, dat$adjoint$update$unpack)
  body$add(
    generate_dust_unpack(unpack, dat$storage$packing$state, dat$sexp_data))

  unpack <- intersect(dat$storage$contents$adjoint,
                      dat$adjoint$update$unpack_adjoint)
  body$add(
    generate_dust_unpack(unpack, dat$storage$packing$adjoint, dat$sexp_data,
                         "adjoint"))
  eqs <- c(get_phase_equations("update", dat, adjoint = TRUE),
           dat$adjoint$update$adjoint)
  for (eq in eqs) {
    body$add(generate_dust_assignment(eq, "adjoint_next", dat))
  }

  cpp_function("void", "adjoint_update", args, body$get(), static = TRUE)
}


generate_dust_system_adjoint_compare_data <- function(dat) {
  args <- c("real_type" = "time",
            "const real_type*" = "state",
            "const real_type*" = "adjoint",
            "const data_type&" = "data",
            "const shared_state&" = "shared",
            "internal_state&" = "internal",
            "real_type*" = "adjoint_next")
  body <- collector()

  unpack <- intersect(dat$variables, dat$adjoint$compare$unpack)
  body$add(
    generate_dust_unpack(unpack, dat$storage$packing$state, dat$sexp_data))

  unpack <- intersect(dat$storage$contents$adjoint,
                      dat$adjoint$compare$unpack_adjoint)
  body$add(
    generate_dust_unpack(unpack, dat$storage$packing$adjoint, dat$sexp_data,
                         "adjoint"))

  eqs <- c(get_phase_equations("compare", dat, adjoint = TRUE),
           dat$adjoint$compare$adjoint)
  for (eq in eqs) {
    body$add(generate_dust_assignment(eq, "adjoint_next", dat))
  }

  cpp_function("void", "adjoint_compare_data", args, body$get(), static = TRUE)
}


generate_dust_system_adjoint_initial <- function(dat) {
  args <- c("real_type" = "time",
            "const real_type*" = "state",
            "const real_type*" = "adjoint",
            "const shared_state&" = "shared",
            "internal_state&" = "internal",
            "real_type*" = "adjoint_next")
  body <- collector()

  unpack <- intersect(dat$variables, dat$adjoint$initial$unpack)
  body$add(
    generate_dust_unpack(unpack, dat$storage$packing$state, dat$sexp_data))

  unpack <- intersect(dat$storage$contents$adjoint,
                      dat$adjoint$initial$unpack_adjoint)
  body$add(
    generate_dust_unpack(unpack, dat$storage$packing$adjoint, dat$sexp_data,
                         "adjoint"))

  eqs <- c(get_phase_equations("initial", dat, adjoint = TRUE),
           dat$adjoint$initial$adjoint)
  for (eq in eqs) {
    body$add(generate_dust_assignment(eq, "adjoint_next", dat))
  }

  cpp_function("void", "adjoint_initial", args, body$get(), static = TRUE)
}


generate_dust_lhs <- function(lhs, dat, name_state, options) {
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
      stop("We don't have stack-allocated arrays")
    }
    sprintf("const %s %s", dat$storage$type[[name]], name)
  } else if (location %in% c("shared", "internal")) {
    if (is_array) {
      stopifnot(!(name %in% dat$parameters$name))
      sprintf("%s[%s]",
              generate_dust_sexp(name, dat$sexp_data, options),
              generate_dust_sexp(idx, dat$sexp_data, options))
    } else if (location == "shared" && isFALSE(options$shared_exists)) {
      ## Can use 'name' for last arg
      sprintf("const %s %s",
              dat$storage$type[[name]],
              generate_dust_sexp(name, dat$sexp_data, options))
    } else {
      generate_dust_sexp(name, dat$sexp_data, options)
    }
  } else if (location == "state") {
    offset <- call("OdinOffset", "state", name)
    if (is_array) {
      offset <- expr_plus(idx, offset)
    }
    sprintf("%s[%s]",
            name_state,
            generate_dust_sexp(offset, dat$sexp_data, options))
  } else if (location == "adjoint") {
    offset <- call("OdinOffset", "adjoint", name)
    if (is_array) {
      stop("arrays in adjoint probably need checking carefully")
      offset <- expr_plus(idx, offset)
    }
    sprintf("%s[%s]",
            name_state,
            generate_dust_sexp(offset, dat$sexp_data, options))
  } else {
    stop(sprintf("Unsupported location '%s'", location))
  }
}


generate_dust_assignment <- function(eq, name_state, dat, options = list()) {
  if (eq$rhs$type == "parameter") {
    name <- eq$lhs$name
    type <- dat$storage$type[[name]]
    read <- if (type == "real_type") "read_real" else sprintf("read_%s", type)
    type <- if (eq$rhs$args$type == "real_type") "real" else eq$rhs$args$type
    is_array <- name %in% dat$storage$arrays$name
    if (is_array) {
      i <- match(name, dat$storage$arrays$name)
      if (isFALSE(options$shared_exists)) {
        dim <- sprintf("dim_%s", name)
        required <- "true"
      } else {
        dim <- sprintf("shared.dim.%s", name)
        required <- "false"
      }
      dest <- generate_dust_sexp(name, dat$sexp_data, options)
      res <- sprintf(
        'dust2::r::%s_array(parameters, %s, %s.data(), "%s", %s);',
        read, dim, dest, name, required)
    } else {
      lhs <- generate_dust_lhs(eq$lhs, dat, name_state, options)

      if (isFALSE(options$shared_exists)) {
        if (is.null(eq$rhs$args$default)) {
          rhs <- sprintf('dust2::r::%s(parameters, "%s")', read, eq$lhs$name)
        } else {
          default <- generate_dust_sexp(
            eq$rhs$args$default, dat$sexp_data, options)
          rhs <- sprintf('dust2::r::%s(parameters, "%s", %s)',
                         read, eq$lhs$name, default)
        }
      } else {
        rhs <- sprintf('dust2::r::%s(parameters, "%s", %s)',
                       read, eq$lhs$name, lhs)
      }
      res <- sprintf("%s = %s;", lhs, rhs)
    }
  } else if (identical(eq$special, "dim")) {
    i <- match(eq$lhs$name_data, dat$storage$arrays$name)
    dims <- vcapply(dat$storage$arrays$dims[[i]], generate_dust_sexp,
                    dat$sexp_data, options)
    dims_str <- paste(sprintf("static_cast<size_t>(%s)", dims), collapse = ", ")
    res <- sprintf("const dust2::array::dimensions<%d> %s{%s};",
                   dat$storage$arrays$rank[[i]],
                   eq$lhs$name,
                   dims_str)
  } else {
    is_array <- !is.null(eq$lhs$array)
    lhs <- generate_dust_lhs(eq$lhs, dat, name_state, options)
    rhs <- generate_dust_sexp(eq$rhs$expr, dat$sexp_data, options)

    res <- sprintf("%s = %s;", lhs, rhs)
    is_array <- !is.null(eq$lhs$array)
    if (is_array) {
      for (idx in rev(eq$lhs$array)) {
        if (idx$type == "range") {
          from <- generate_dust_sexp(idx$from, dat$sexp_data, options)
          to <- generate_dust_sexp(idx$to, dat$sexp_data, options)
          res <- c(sprintf("for (size_t %s = %s; %s <= %s; ++%s) {",
                           idx$name, from, idx$name, to, idx$name),
                   res,
                   "}")
        }
      }
      if (length(res) > 1) {
        n <- (length(res) - 1) / 2
        indent <- strrep("  ", c(seq_len(n + 1), rev(seq_len(n))) - 1)
        res <- paste0(indent, res)
      }
    }
  }

  res
}


generate_dust_unpack <- function(names, packing, sexp_data, from = "state") {
  i <- match(names, packing$name)
  is_scalar <- lengths(packing$dims[i]) == 0
  is_array <- !is_scalar

  offset <- vcapply(packing$name[i], function(nm) {
    generate_dust_sexp(call("OdinOffset", from, nm), sexp_data)
  })
  ret <- character(length(names))
  ret[is_scalar] <- sprintf("const auto %s = %s[%s];",
                            names[is_scalar],
                            from,
                            offset[is_scalar])
  ret[is_array] <- sprintf("const auto * %s = %s + %s;",
                           names[is_array],
                           from,
                           offset[is_array])
  ret
}


get_phase_equations <- function(phase, dat, adjoint = FALSE) {
  if (adjoint) {
    nms <- dat$adjoint[[phase]]$equations
  } else {
    nms <- dat$phases[[phase]]$equations
  }
  dat$equations[names(dat$equations) %in% nms]
}
