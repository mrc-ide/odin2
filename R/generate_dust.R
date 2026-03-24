generate_dust_system <- function(dat) {
  dat <- generate_prepare(dat)

  body <- collector()
  body$add(generate_dust_system_includes(dat))
  body$add(generate_dust_system_attributes(dat))
  body$add(sprintf("class %s {", dat$class))
  body$add("public:")
  body$add(sprintf("  %s() = delete;", dat$class))
  body$add("  using real_type = double;")
  body$add("  using rng_state_type = monty::random::generator<real_type>;")
  parts <- generate_dust_parts()
  for (nm in names(parts)) {
    body$add(sprintf("  %s", parts[[nm]](dat)))
  }
  body$add("};")
  body$get()
}


generate_dust_parts <- function() {
  list(
    shared_state = generate_dust_system_shared_state,
    internal_state = generate_dust_system_internal_state,
    data_type = generate_dust_system_data_type,
    packing_state = generate_dust_system_packing_state,
    packing_gradient = generate_dust_system_packing_gradient,
    build_shared = generate_dust_system_build_shared,
    build_internal = generate_dust_system_build_internal,
    build_data = generate_dust_system_build_data,
    update_shared = generate_dust_system_update_shared,
    update_internal = generate_dust_system_update_internal,
    initial = generate_dust_system_initial,
    update = generate_dust_system_update,
    rhs = generate_dust_system_rhs,
    output = generate_dust_system_output,
    delays = generate_dust_system_delays,
    size_output = generate_dust_system_size_output,
    zero_every = generate_dust_system_zero_every,
    compare_data = generate_dust_system_compare_data,
    adjoint = generate_dust_system_adjoint)
}


generate_prepare <- function(dat) {
  rank <- set_names(dat$storage$arrays$rank, dat$storage$arrays$name)
  alias <- set_names(dat$storage$arrays$alias, dat$storage$arrays$name)
  dat$sexp_data <- generate_dust_dat(dat$storage$location,
                                     dat$storage$packing,
                                     dat$storage$type,
                                     dat$storage$arrays)
  dat
}


generate_dust_system_includes <- function(dat) {
  c("#include <dust2/common.hpp>",
    if (!is.null(dat$browser)) "#include <dust2/r/browser.hpp>")
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
  pars <- dat$parameters
  keep <- c("type", "rank", "required", "constant")
  str <- matrix(
    unlist0(lapply(pars[keep], vcapply, deparse, control = NULL)),
    ncol = length(keep))
  args <- apply(str, 1, function(el) {
    paste(sprintf("%s = %s", keep, el), collapse = ", ")
  })
  parameters <- sprintf("// [[dust2::parameter(%s, %s)]]", pars$name, args)
  c(sprintf("// [[dust2::class(%s)]]", dat$class),
    sprintf("// [[dust2::time_type(%s)]]", dat$time),
    has_compare,
    has_adjoint,
    parameters)
}


generate_dust_system_shared_state <- function(dat) {
  nms <- dat$storage$contents$shared
  type <- dat$storage$type[nms]
  is_array <- nms %in% dat$storage$arrays$name
  type[is_array] <- sprintf("std::vector<%s>", type[is_array])

  is_interpolator <- type == "interpolator"
  if (any(is_interpolator)) {
    type[is_interpolator] <- vcapply(
      dat$equations[names(which(is_interpolator))], function(eq) {
        rank <- eq$rhs$expr$rank
        mode <- eq$rhs$expr$mode
        substr(mode, 1, 1) <- toupper(substr(mode, 1, 1))
        if (rank == 0) {
          sprintf("dust2::interpolate::Interpolate%s<real_type>", mode)
        } else {
          sprintf("dust2::interpolate::Interpolate%sArray<real_type, %d>",
                  mode, rank)
        }
      })
  }

  not_aliased <- dat$storage$arrays$alias == dat$storage$arrays$name
  if (any(not_aliased)) {
    dims <- c(
      "struct dim_type {",
      sprintf("  dust2::array::dimensions<%d> %s;",
              dat$storage$arrays$rank[not_aliased],
              dat$storage$arrays$name[not_aliased]),
      "} dim;")
  } else {
    dims <- NULL
  }

  has_adjoint <- !is.null(dat$adjoint)
  packing_type <- c("state", if (has_adjoint) c("adjoint", "gradient"))
  packing_size <- viapply(dat$storage$packing[packing_type], nrow)

  c("struct shared_state {",
    "  struct odin_internals_type {",
    "    struct {",
    sprintf("      dust2::packing %s;", packing_type),
    "    } packing;",
    "    struct {",
    sprintf("      std::array<size_t, %d> %s;",
            packing_size, packing_type),
    "    } offset;",
    "  } odin;",
    sprintf("  %s", dims),
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
    type <- ifelse(data %in% dat$storage$arrays$name,
                   "std::vector<real_type>", "real_type")
    c("struct data_type {",
      sprintf("  %s %s;", type, data),
      "};")
  }
}


generate_dust_system_packing_state <- function(dat) {
  args <- c("const shared_state&" = "shared")
  body <- "return shared.odin.packing.state;"
  cpp_function("dust2::packing", "packing_state", args, body, static = TRUE)
}


generate_dust_system_packing_gradient <- function(dat) {
  if (is.null(dat$adjoint)) {
    return(NULL)
  }
  args <- c("const shared_state&" = "shared")
  body <- "return shared.odin.packing.gradient;"
  cpp_function("dust2::packing", "packing_gradient", args, body, static = TRUE)
}


generate_dust_system_packing <- function(name, dat) {
  packing <- dat$storage$packing[[name]]
  arrays <- dat$storage$arrays
  fmt <- "std::vector<size_t>(dim.%s.dim.begin(), dim.%s.dim.end())"
  dim_name <- arrays$alias[match(packing$name, arrays$name)]
  dims <- ifelse(packing$rank == 0, "{}", sprintf(fmt, dim_name, dim_name))
  els <- sprintf('{"%s", %s}', packing$name, dims)
  ## trailing comma if needed
  els[-length(els)] <- sprintf("%s,", els[-length(els)])
  c(sprintf("odin.packing.%s = dust2::packing{", name),
    sprintf("  %s", els),
    "};")
}


generate_dust_system_build_shared <- function(dat) {
  options <- list(shared_exists = FALSE)
  body <- collector()
  if (nrow(dat$storage$arrays) > 0) {
    body$add("shared_state::dim_type dim;");
  }
  eqs <- get_phase_equations("build_shared", dat)
  allocated <- character()
  for (eq in eqs) {
    name <- eq$lhs$name
    if (name %in% dat$storage$arrays$name && !(name %in% allocated)) {
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
    nms_return <- c("odin", "dim",
                    setdiff(dat$phases$build_shared$equations, nms_dim))
  } else {
    nms_return <- c("odin", dat$phases$build_shared$equations)
  }

  body$add("shared_state::odin_internals_type odin;")
  has_adjoint <- !is.null(dat$adjoint)
  packing_type <- c("state", if (has_adjoint) c("adjoint", "gradient"))
  for (type in packing_type) {
    body$add(generate_dust_system_packing(type, dat))
  }
  body$add(sprintf(
    "odin.packing.%s.copy_offset(odin.offset.%s.begin());",
    packing_type, packing_type))
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

  for (name in data) {
    if (name %in% dat$storage$arrays$name) {
      ## Some duplication here with generate_dust_assignment, but we
      ## don't have an "equation" here for the data element to work
      ## with.
      size <- generate_dust_sexp(
        call("OdinLength", name),
        dat$sexp_data, list())
      i <- match(name, dat$storage$arrays$name)
      dim_name <- dat$storage$arrays$alias[i]
      dim <- sprintf("shared.dim.%s", dim_name)
      body$add(sprintf("auto %s = std::vector<real_type>(%s);", name, size))
      body$add(sprintf(
        'dust2::r::read_real_array(data, %s, %s.data(), "%s", true);',
        dim, name, name))
    } else {
      body$add(sprintf('auto %s = dust2::r::read_real(data, "%s", NA_REAL);',
                       name, name))
    }
  }
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

  body$add(generate_dust_print(dat, "update"))
  body$add(generate_dust_browser(dat, "update"))

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

  if (any(dat$delays$in_rhs)) {
    arg_delays <- c(
      "const dust2::ode::delay_result_type<real_type>&" = "delays")
    args <- append(args, arg_delays, after = 4)
    body$add(generate_dust_system_delay_equations("rhs", dat))
  }

  unpack <- intersect(dat$variables, dat$phases$deriv$unpack)
  body$add(
    generate_dust_unpack(unpack, dat$storage$packing$state, dat$sexp_data))
  eqs <- c(get_phase_equations("deriv", dat),
           dat$phases$deriv$variables)
  for (eq in eqs) {
    body$add(generate_dust_assignment(eq, "state_deriv", dat))
  }
  
  body$add(generate_dust_print(dat, "deriv"))
  body$add(generate_dust_browser(dat, "deriv"))

  cpp_function("void", "rhs", args, body$get(), static = TRUE)
}


generate_dust_system_output <- function(dat) {
  if (length(dat$output) == 0) {
    return(NULL)
  }
  args <- c("real_type" = "time",
            "real_type*" = "state",
            "const shared_state&" = "shared",
            "internal_state&" = "internal")

  body <- collector()

  if (any(dat$delays$in_output)) {
    arg_delays <- c(
      "const dust2::ode::delay_result_type<real_type>&" = "delays")
    args <- c(args, arg_delays)
    body$add(generate_dust_system_delay_equations("output", dat))
  }

  unpack <- intersect(dat$variables, dat$phases$output$unpack)
  body$add(
    generate_dust_unpack(unpack, dat$storage$packing$state, dat$sexp_data))
  eqs <- get_phase_equations("output", dat)
  for (eq in eqs) {
    body$add(generate_dust_assignment(eq, "state", dat))
  }

  packing <- dat$storage$packing$state
  for (el in dat$output) {
    src <- generate_dust_sexp(el, dat$sexp_data)
    offset <- generate_dust_sexp(call("OdinOffset", "state", el),
                                 dat$sexp_data)
    is_array <- el %in% dat$storage$arrays$name
    if (is_array) {
      body$add(sprintf(
        "std::copy(%s.begin(), %s.end(), state + %s);",
        src, src, offset))
    } else {
      body$add(sprintf("state[%s] = %s;", offset, src))
    }
  }

  cpp_function("void", "output", args, body$get(), static = TRUE)
}


generate_dust_system_delays <- function(dat) {
  if (is.null(dat$delays)) {
    return(NULL)
  }
  args <- c("const shared_state&" = "shared")
  body <- collector()

  delays <- dat$delays
  arrays <- dat$storage$arrays

  for (i in seq_len(nrow(delays))) {
    nm <- delays$name[[i]]
    by <- generate_dust_sexp(delays$by[[i]], dat$sexp_data)
    variables <- delays$value[[i]]$variables
    offset <- vcapply(variables, function(v) {
      generate_dust_sexp(call("OdinOffset", "state", v), dat$sexp_data)
    })
    size <- vcapply(variables, function(v) {
      if (v %in% arrays$name) {
        generate_dust_sexp(call("OdinLength", v), dat$sexp_data)
      } else {
        "1"
      }
    })
    index <- sprintf("{%s, %s}", offset, size)
    body$add(sprintf("const dust2::ode::delay<real_type> %s(%s, {%s});",
                     nm, by, paste(index, collapse = ", ")))
  }

  body$add(sprintf("return dust2::ode::delays<real_type>({%s});",
                   paste(delays$name, collapse = ", ")))

  cpp_function("auto", "delays", args, body$get(), static = TRUE)
}


generate_dust_system_size_output <- function(dat) {
  if (length(dat$output) == 0) {
    return(NULL)
  }
  args <- list()
  body <- sprintf("return %d;", length(dat$output))
  cpp_function("size_t", "size_output", args, body, static = TRUE)
}


generate_dust_system_zero_every <- function(dat) {
  if (is.null(dat$zero_every)) {
    return(NULL)
  }
  args <- c("const shared_state&" = "shared")
  packing <- dat$storage$packing$state
  every <- vcapply(dat$zero_every, generate_dust_sexp, dat$sexp_data,
                   USE.NAMES = FALSE)
  i <- match(names(dat$zero_every), packing$name)
  is_array <- packing$rank[i] > 0
  nms <- sprintf("zero_every_%s", packing$name[i])

  offset <- lapply(nms, function(name) {
    call("OdinOffset", "state", packing$name[i])
  })
  index <- vcapply(packing$name[i], function(name) {
    offset <- generate_dust_sexp(call("OdinOffset", "state", name),
                                 dat$sexp_data)
    if (name %in% dat$storage$arrays$name) {
      size <- generate_dust_sexp(call("OdinLength", name), dat$sexp_data)
      sprintf("{dust2::tools::integer_sequence(%s, %s)}", size, offset)
    } else {
      sprintf("{%s}", offset)
    }
  })
  str <- paste(sprintf("{%s, %s}", every, index), collapse = ", ")
  body <- sprintf("return dust2::zero_every_type<real_type>{%s};", str)
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
  ## We could happily pop this into monty I think, as this is a
  ## generally reasonable pattern
  body$add(
    "auto unless_nan = [](real_type x) { return std::isnan(x) ? 0 : x; };")
  body$add(
    generate_dust_unpack(unpack, dat$storage$packing$state, dat$sexp_data))
  body$add("real_type odin_ll = 0;")

  eqs <- get_phase_equations("compare", dat)
  for (eq in eqs) {
    body$add(generate_dust_assignment(eq, "state", dat))
  }

  ## Then the actual comparison:
  for (eq in dat$phases$compare$compare) {
    is_array <- !is.null(eq$array)

    eq_args <- vcapply(eq$rhs$args, generate_dust_sexp, dat$sexp_data)
    eq_str <- sprintf("odin_ll += unless_nan(monty::density::%s(%s, true));",
                      eq$rhs$density$cpp, paste(eq_args, collapse = ", "))

    if (is_array) {
      options <- NULL
      depends <- find_dependencies(eq$rhs$expr)$variables
      eq_str <- generate_array_loops(
        eq_str, depends, eq$array, dat$sexp_data, options)
    }
    body$add(eq_str)
  }

  body$add("return odin_ll;")
  cpp_function("real_type", "compare_data", args, body$get(), static = TRUE)
}


generate_dust_system_adjoint <- function(dat) {
  if (is.null(dat$adjoint)) {
    return(NULL)
  }

  c(generate_dust_system_adjoint_update(dat),
    generate_dust_system_adjoint_compare_data(dat),
    generate_dust_system_adjoint_initial(dat),
    generate_dust_system_adjoint_rhs(dat))
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
  options <- list(stochastic_expectation = TRUE)

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
    body$add(generate_dust_assignment(eq, "adjoint_next", dat, options))
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
  options <- list(stochastic_expectation = TRUE)

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
    body$add(generate_dust_assignment(eq, "adjoint_next", dat, options))
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
  options <- list(stochastic_expectation = TRUE)

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
    body$add(generate_dust_assignment(eq, "adjoint_next", dat, options))
  }

  cpp_function("void", "adjoint_initial", args, body$get(), static = TRUE)
}


## Generate adjoint_rhs for continuous models.  This computes the
## derivatives of the adjoint vector: dλ/dt = -(∂f/∂y)^T λ for state
## adjoints and dμ/dt = -(∂f/∂θ)^T λ for parameter adjoints.  The
## negative sign implements the adjoint equation for backward time.
generate_dust_system_adjoint_rhs <- function(dat) {
  if (is.null(dat$adjoint$deriv)) {
    return(NULL)
  }
  args <- c("real_type" = "time",
            "const real_type*" = "state",
            "const real_type*" = "adjoint",
            "const shared_state&" = "shared",
            "internal_state&" = "internal",
            "real_type*" = "adjoint_deriv")
  body <- collector()
  options <- list(stochastic_expectation = TRUE)

  unpack <- intersect(dat$variables, dat$adjoint$deriv$unpack)
  body$add(
    generate_dust_unpack(unpack, dat$storage$packing$state, dat$sexp_data))

  unpack <- intersect(dat$storage$contents$adjoint,
                      dat$adjoint$deriv$unpack_adjoint)
  body$add(
    generate_dust_unpack(unpack, dat$storage$packing$adjoint, dat$sexp_data,
                         "adjoint"))

  eqs <- c(get_phase_equations("deriv", dat, adjoint = TRUE),
           dat$adjoint$deriv$adjoint)
  for (eq in eqs) {
    body$add(generate_dust_assignment(eq, "adjoint_deriv", dat, options))
  }

  cpp_function("void", "adjoint_rhs", args, body$get(), static = TRUE)
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
      stop("We don't have stack-allocated arrays") # nocov
    }
    sprintf("const %s %s", dat$storage$type[[name]], name)
  } else if (location %in% c("shared", "internal")) {
    if (is_array) {
      stopifnot(!(name %in% dat$parameters$name))
      sprintf("%s[%s]",
              generate_dust_sexp(name, dat$sexp_data, options),
              generate_dust_sexp(idx, dat$sexp_data, options))
    } else if (location == "shared" && isFALSE(options$shared_exists)) {
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
    type <- dat$parameters$type[dat$parameters$name == name]
    read <- if (type == "real_type") "read_real" else sprintf("read_%s", type)
    is_array <- name %in% dat$storage$arrays$name
    if (isFALSE(options$shared_exists)) {
      dest <- name
    } else {
      dest <- sprintf("shared.%s", name)
    }
    if (is_array) {
      i <- match(name, dat$storage$arrays$name)
      dim_name <- dat$storage$arrays$alias[i]
      if (isFALSE(options$shared_exists)) {
        dim <- sprintf("dim.%s", dim_name)
        required <- "true"
      } else {
        dim <- sprintf("shared.dim.%s", dim_name)
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
                       read, eq$lhs$name, dest)
      }
      res <- sprintf("%s = %s;", lhs, rhs)
    }
    for (constraint in c("min", "max")) {
      if (!is.null(eq$rhs$args[[constraint]])) {
        what <- if (is_array) "array" else "scalar"
        constraint_value <- generate_dust_sexp(eq$rhs$args[[constraint]],
                                               dat$sexp_data, options)
        res <- c(res,
                 sprintf('dust2::r::check_%s_%s<%s>(%s, %s, "%s");',
                         constraint, what, type, dest, constraint_value, name))
      }
    }
  } else if (identical(eq$special, "dim")) {
    i <- match(eq$lhs$name_data, dat$storage$arrays$name)
    if (dat$storage$arrays$name[i] != dat$storage$arrays$alias[i]) {
      return(NULL)
    }
    rank <- dat$storage$arrays$rank[[i]]
    if (eq$rhs$is_user_sized) {
      res <- sprintf(
        'dim.%s = dust2::r::read_dimensions<%d>(parameters, "%s");',
        eq$lhs$name_data, rank, eq$lhs$name_data)
    } else {
      dims <- vcapply(dat$storage$arrays$dims[[i]], generate_dust_sexp,
                      dat$sexp_data, options)
      dims_str <- paste(sprintf("static_cast<size_t>(%s)", dims),
                        collapse = ", ")
      res <- sprintf("dim.%s.set({%s});", eq$lhs$name_data, dims_str)
    }
  } else if (identical(eq$rhs$type, "interpolate")) {
    name <- eq$lhs$name
    rhs <- generate_dust_sexp(eq$rhs$expr, dat$sexp_data, options)
    if (isFALSE(options$shared_exists)) {
      dest <- name
      res <- sprintf("const auto %s = %s;", dest, rhs)
    } else {
      dest <- sprintf("shared.%s", name)
      res <- sprintf("%s = %s;", dest, rhs)
    }
  } else if (rlang::is_call(eq$rhs$expr, "OdinInterpolateEval") &&
             eq$lhs$name %in% dat$storage$arrays$name) {
    ## Special case here until we sort out vector valued functions I
    ## think.
    res <- sprintf("%s;",
                   generate_dust_sexp(eq$rhs$expr, dat$sexp_data, options))
  } else if (identical(eq$rhs$type, "delay")) {
    ## The equation will be generated separately
    res <- NULL
  } else {
    lhs <- generate_dust_lhs(eq$lhs, dat, name_state, options)
    rhs <- generate_dust_sexp(eq$rhs$expr, dat$sexp_data, options)

    res <- sprintf("%s = %s;", lhs, rhs)
    is_array <- !is.null(eq$lhs$array)
    if (is_array) {
      depends <- find_dependencies(eq$rhs$expr)$variables
      ## Check for free index variables in the RHS that are not
      ## covered by the LHS array loops.  This happens in adjoint
      ## equations when a lower-dimensional variable depends on a
      ## higher-dimensional one: e.g., adj_prop_infectious[i] gets a
      ## contribution from s_ij_sex[i, j] which introduces free 'j'.
      ## We need to wrap in a reduction loop and accumulate with +=.
      lhs_indices <- vcapply(eq$lhs$array, function(x) x$name)
      free_indices <- adjoint_find_free_indices(
        eq$rhs$expr, lhs_indices, dat$storage$arrays)
      if (length(free_indices) > 0) {
        ## Before generating the reduction loop, check for Kronecker
        ## deltas of the form (free == lhs_idx ? 1 : 0).  These arise
        ## when a lower-dim adjoint (adj_prop_I[i]) is accumulated from
        ## a higher-dim variable (s_ij[i, j]) where the dependency is
        ## through a mismatched index (prop_I[j]).  The delta collapses
        ## the sum to a single term, but the index roles in the array
        ## subscripts need swapping.  E.g.,
        ##   adj_s_ij[i, j] * m[i, j] * (j == i ? 1 : 0)
        ## becomes
        ##   adj_s_ij[j, i] * m[j, i]
        ## with j still as the reduction variable.
        rhs_expr <- eq$rhs$expr
        rhs_expr <- adjoint_resolve_kronecker_delta(
          rhs_expr, lhs_indices, free_indices)
        rhs <- generate_dust_sexp(rhs_expr, dat$sexp_data, options)
        depends <- find_dependencies(rhs_expr)$variables

        ## Build reduction loops for the free indices.
        ## Zero-init the LHS, then accumulate contributions.
        lhs_assign <- sprintf("%s = 0;", lhs)
        inner_body <- sprintf("%s += %s;", lhs, rhs)
        inner <- generate_array_loops(
          inner_body, depends, free_indices, dat$sexp_data, options)
        res <- c(lhs_assign, inner)
        res <- generate_array_loops(
          res, depends, eq$lhs$array, dat$sexp_data, options)
      } else {
        res <- generate_array_loops(
          res, depends, eq$lhs$array, dat$sexp_data, options)
      }
    } else if (!is.null(eq$reduce_loops)) {
      ## Scalar adjoint accumulating from array equations.  Split
      ## the expression into the accumulate (old value) part and
      ## the array-indexed contributions, then generate:
      ##   lhs = accumulate_part;
      ##   for (...) { lhs += array_contribution; }
      ## For stack-location intermediates, lhs includes "const real_type"
      ## declaration which can't be used with +=. Strip it for the
      ## loop body and use non-const declaration for init.
      location <- dat$storage$location[[eq$lhs$name]]
      if (identical(location, "stack")) {
        lhs_bare <- eq$lhs$name
        lhs_decl <- sprintf("%s %s",
                            dat$storage$type[[eq$lhs$name]], lhs_bare)
      } else {
        lhs_bare <- lhs
        lhs_decl <- lhs
      }
      accum_expr <- adjoint_split_accumulate(eq$rhs$expr, eq$lhs$name)
      if (!is.null(accum_expr$base)) {
        base_rhs <- generate_dust_sexp(accum_expr$base, dat$sexp_data, options)
        inner_rhs <- generate_dust_sexp(accum_expr$contrib, dat$sexp_data,
                                        options)
        depends <- find_dependencies(accum_expr$contrib)$variables
        loop_body <- sprintf("%s += %s;", lhs_bare, inner_rhs)
        loop <- generate_array_loops(
          loop_body, depends, eq$reduce_loops, dat$sexp_data, options)
        res <- c(sprintf("%s = %s;", lhs_decl, base_rhs), loop)
      } else {
        ## Pure reduction, no accumulate base
        inner_rhs <- generate_dust_sexp(eq$rhs$expr, dat$sexp_data, options)
        depends <- find_dependencies(eq$rhs$expr)$variables
        loop_body <- sprintf("%s += %s;", lhs_bare, inner_rhs)
        loop <- generate_array_loops(
          loop_body, depends, eq$reduce_loops, dat$sexp_data, options)
        res <- c(sprintf("%s = 0;", lhs_decl), loop)
      }
    }
  }

  res
}


generate_dust_unpack <- function(names, packing, sexp_data, from = "state") {
  if (length(names) == 0) {
    return(NULL)
  }
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


generate_dust_print_statement <- function(eq, dat) {
  format <- vcapply(eq$inputs, function(p) {
    if (!is.null(p$format)) {
      paste0("%", p$format)
    } else if (rlang::is_call(p$expr, "as.integer")) {
      "%d"
    } else if (!is.name(p$expr)) {
      "%f"
    } else {
      nm <- as.character(p$expr)
      if (dat$storage$type[[nm]] %in% c("bool", "int")) {
        "%d"
      } else {
        "%f"
      }
    }
  })

  n <- counter()
  transformer <- function(text, envir) {
    format[[n()]]
  }
  fmt <- glue::glue(eq$string, .transformer = transformer)
  args <- vcapply(eq$inputs, function(p) {
    generate_dust_sexp(p$expr, dat$sexp_data)
  })

  str <- sprintf('Rprintf("[%%f] %s\\n", time, %s);',
                 fmt, paste(args, collapse = ", "))
  if (!is.null(eq$when)) {
    when <- generate_dust_sexp(eq$when, dat$sexp_data)
    str <- c(sprintf("if (%s) {", when),
             sprintf("  %s", str),
             "}")
  }
  str
}


get_phase_equations <- function(phase, dat, adjoint = FALSE) {
  if (adjoint) {
    nms <- dat$adjoint[[phase]]$equations
  } else {
    nms <- dat$phases[[phase]]$equations
  }
  dat$equations[names(dat$equations) %in% nms]
}


generate_dust_print <- function(dat, phase) {
  if (is.null(dat$print[[phase]])) {
    return(NULL)
  }

  body <- collector()
  body$add(generate_dust_unpack(dat$print[[phase]]$unpack,
                                dat$storage$packing$state,
                                dat$sexp_data))
  for (eq in dat$print[[phase]]$equations) {
    body$add(generate_dust_print_statement(eq, dat))
  }
  body$get()
}


generate_dust_browser <- function(dat, phase) {
  if (is.null(dat$browser[[phase]])) {
    return()
  }
  body <- collector()
  body$add(generate_dust_unpack(dat$browser[[phase]]$unpack,
                                dat$storage$packing$state,
                                dat$sexp_data))
  env <- "odin_env"
  body$add(sprintf("auto %s = dust2::r::browser::create();", env))

  type <- dat$storage$type
  export <- intersect(c(dat$phases$build_shared$equations,
                        dat$phases$update_shared$equations,
                        dat$phases[[phase]]$equations,
                        dat$variables),
                      names(type)[type %in% c("real_type", "int", "bool")])
  for (v in c("time", export)) {
    body$add(generate_dust_browser_to_env(v, dat, env))
  }
  body$add(sprintf('dust2::r::browser::enter(%s, "%s", time);', env, phase))

  body <- body$get()
  if (!is.null(dat$browser[[phase]]$when)) {
    when <- generate_dust_sexp(dat$browser[[phase]]$when, dat$sexp_data)
    body <- c(sprintf("if (%s) {", when),
              sprintf("  %s", body),
              "}")
  }
  body
}


generate_dust_browser_to_env <- function(name, dat, env) {
  data <- generate_dust_sexp(name, dat$sexp_data)
  arrays <- dat$storage$arrays
  if (name %in% arrays$name) {
    location <- dat$storage$location[[name]]
    if (location %in% c("shared", "internal")) {
      data <- sprintf("%s.data()", data)
    }
    dim <- sprintf("shared.dim.%s", arrays$alias[match(name, arrays$name)])
    sprintf('dust2::r::browser::save(%s, %s, "%s", %s);', data, dim, name, env)
  } else {
    sprintf('dust2::r::browser::save(%s, "%s", %s);', data, name, env)
  }
}


generate_dust_system_delay_equations <- function(phase, dat) {
  delays <- dat$delays$name[dat$delays[[sprintf("in_%s", phase)]]]
  unlist0(lapply(delays, generate_dust_system_delay_equation, dat))
}


generate_dust_system_delay_equation <- function(nm, dat) {
  i <- match(nm, dat$delays$name)
  delay_type <- dat$delays$type[[i]]
  is_array <- nm %in% dat$storage$arrays$name

  if (delay_type == "variable") {
    if (is_array) {
      ret <- sprintf("const auto& %s = delays[%d].data;",
                     nm, i - 1)
    } else {
      ret <- sprintf("const auto %s = delays[%d].data[0];",
                     nm, i - 1)
    }
  } else {
    what <- generate_dust_sexp(dat$delays$value[[i]]$what, dat$sexp_data)
    if (is_array) {
      declare <- NULL
      size <- generate_dust_sexp(call("OdinLength", nm), dat$sexp_data)
      assign <- sprintf("std::copy_n(%s.data(), %s, %s.data());",
                        what, size, generate_dust_sexp(nm, dat$sexp_data))
    } else {
      declare <- sprintf("%s %s;", dat$storage$type[[nm]], nm)
      assign <- sprintf("%s = %s;", nm, what)
    }

    body <- collector()
    variables <- dat$delays$value[[i]]$variables
    for (j in seq_along(variables)) {
      v <- variables[[j]]
      offset <- sprintf("delays[%d].offset[%d]", i - 1, j - 1)
      if (v %in% dat$storage$arrays$name) {
        body$add(sprintf("const auto* %s = delays[%d].data.data() + %s;",
                         v, i - 1, offset))
      } else {
        body$add(sprintf("const auto %s = delays[%d].data[%s];",
                         v, i - 1, offset))
      }
    }

    eqs <- dat$delays$value[[i]]$equations
    for (eq in dat$equations[names(dat$equations) %in% eqs]) {
      body$add(generate_dust_assignment(eq, "state_deriv", dat))
    }
    body$add(assign)
    ret <- c(declare, cpp_body(body$get()))
  }

  ret
}


## Split an accumulated adjoint expression like (adj_beta + contrib)
## into its base (adj_beta) and the array-indexed contribution.
## The accumulate pattern in adjoint_equation adds as.name(name) to
## the parts list, so the expression is typically:
##   name + array_contribution
## where array_contribution contains loop variables (i, j, etc.)
## Find index variables in RHS that are not covered by the LHS array
## loops.  Returns a list of range-index specs suitable for
## generate_array_loops().  Each free index gets a range determined by
## the first array in the expression that uses it.
adjoint_find_free_indices <- function(expr, lhs_indices, arrays_table) {
  ## Collect all array subscript accesses in the expression:
  ## find names used as `X[i, j]` and record which index position
  ## each variable appears in.
  accesses <- list()
  walk_subscripts <- function(e) {
    if (!is.call(e)) return()
    if (identical(e[[1]], as.name("["))) {
      arr_name <- as.character(e[[2]])
      for (k in seq_along(e)[-c(1, 2)]) {
        arg <- e[[k]]
        if (is.name(arg)) {
          idx_name <- as.character(arg)
          if (idx_name %in% INDEX) {
            dim_pos <- k - 2L
            accesses[[length(accesses) + 1L]] <<-
              list(array = arr_name, index = idx_name, dim = dim_pos)
          }
        }
      }
    }
    for (k in seq_len(length(e) - 1L)) {
      tryCatch(
        walk_subscripts(e[[k + 1L]]),
        error = function(cond) NULL)
    }
  }
  walk_subscripts(expr)

  if (length(accesses) == 0) return(list())

  rhs_indices <- unique(vapply(accesses, function(x) x$index, ""))
  free <- setdiff(rhs_indices, lhs_indices)
  if (length(free) == 0) return(list())

  ## For each free index, find a reference array and dimension to
  ## determine its range.
  result <- vector("list", length(free))
  for (fi in seq_along(free)) {
    idx_name <- free[[fi]]
    ref <- NULL
    for (acc in accesses) {
      if (acc$index == idx_name) {
        ref <- acc
        break
      }
    }
    if (is.null(ref)) next
    result[[fi]] <- list(
      name = idx_name,
      type = "range",
      from = 1L,
      to = call("OdinDim", ref$array, as.integer(ref$dim)))
  }
  result[!vapply(result, is.null, FALSE)]
}


## Resolve Kronecker deltas in adjoint expressions with free indices.
##
## When a lower-dimensional adjoint (e.g., adj_prop_I[i]) accumulates
## from a higher-dimensional equation (e.g., s_ij[i,j] = m[i,j] * prop_I[j]),
## the symbolic differentiator produces an expression like:
##
##   adj_s_ij[i, j] * m[i, j] * (j == i ? 1 : 0)
##
## The Kronecker delta (j == i ? 1 : 0) indicates that only terms where
## j matches the LHS index i contribute.  However, in the generated code
## the outer loop variable i (for adj_prop_I[i]) is the same symbol as
## i in adj_s_ij[i, j], creating a name collision.  The correct
## accumulation is:
##
##   adj_prop_I[i] = sum_j adj_s_ij[j, i] * m[j, i]
##
## i.e., the second index of s_ij (matching prop_I's index) takes the
## LHS value i, while the first index becomes the summation variable j.
##
## This function detects the delta, removes it, and swaps the index
## variables in all array subscript accesses within the expression.
adjoint_resolve_kronecker_delta <- function(expr, lhs_indices, free_indices) {
  free_names <- vapply(free_indices, function(x) x$name, "")

  for (fi in seq_along(free_names)) {
    free_idx <- free_names[[fi]]
    for (lhs_idx in lhs_indices) {
      if (free_idx == lhs_idx) next
      ## Look for delta pattern: (free == lhs ? 1 : 0)
      delta <- adjoint_find_delta(expr, free_idx, lhs_idx)
      if (!is.null(delta)) {
        ## Remove the delta from the expression
        expr <- adjoint_remove_delta(expr, delta)
        ## Swap free_idx and lhs_idx in all subscript positions
        expr <- adjoint_swap_subscript_indices(expr, free_idx, lhs_idx)
        break
      }
    }
  }
  expr
}


## Find a Kronecker delta of the form (a == b ? 1 : 0) in an expression.
## Returns the delta sub-expression if found, NULL otherwise.
adjoint_find_delta <- function(expr, idx_a, idx_b) {
  if (!is.call(expr)) return(NULL)
  ## Check if this expression IS a delta: (a == b ? 1 : 0)
  ## In R's AST this is: if(a == b) 1 else 0
  if (identical(expr[[1]], as.name("if")) && length(expr) == 4) {
    cond <- expr[[2]]
    if (is.call(cond) && identical(cond[[1]], as.name("==")) &&
        length(cond) == 3) {
      args <- sort(c(as.character(cond[[2]]), as.character(cond[[3]])))
      target <- sort(c(idx_a, idx_b))
      if (identical(args, target)) {
        true_val <- expr[[3]]
        false_val <- expr[[4]]
        if (is.numeric(true_val) && true_val == 1 &&
            is.numeric(false_val) && false_val == 0) {
          return(expr)
        }
      }
    }
  }
  ## Recurse into children
  for (k in seq_len(length(expr) - 1L)) {
    child <- tryCatch(expr[[k + 1L]], error = function(e) NULL)
    if (!is.null(child)) {
      found <- adjoint_find_delta(child, idx_a, idx_b)
      if (!is.null(found)) return(found)
    }
  }
  NULL
}


## Remove a delta sub-expression from a product.
## When the delta is multiplied in a product (a * b * delta), replace
## it with 1 and simplify.  If it appears in a sum context or alone,
## wrap the remaining expression.
adjoint_remove_delta <- function(expr, delta) {
  if (identical(expr, delta)) return(1)
  if (!is.call(expr)) return(expr)

  ## If this is a multiply and one operand is the delta, remove it
  if (identical(expr[[1]], as.name("*")) && length(expr) == 3) {
    if (identical(expr[[2]], delta)) return(expr[[3]])
    if (identical(expr[[3]], delta)) return(expr[[2]])
  }

  ## Recurse into all children
  result <- expr
  for (k in seq_len(length(expr) - 1L)) {
    child <- tryCatch(expr[[k + 1L]], error = function(e) NULL)
    if (!is.null(child)) {
      new_child <- adjoint_remove_delta(child, delta)
      if (!identical(new_child, child)) {
        result[[k + 1L]] <- new_child
      }
    }
  }
  result
}


## Swap two index variables in all array subscript positions.
## Walks the expression tree and in all X[a, b, ...] accesses,
## swaps occurrences of idx_a and idx_b.
adjoint_swap_subscript_indices <- function(expr, idx_a, idx_b) {
  if (is.name(expr)) return(expr)  # Don't swap bare names
  if (!is.call(expr)) return(expr)

  ## If this is an array subscript access X[...], swap indices in positions
  if (identical(expr[[1]], as.name("["))) {
    sym_a <- as.name(idx_a)
    sym_b <- as.name(idx_b)
    for (k in seq_along(expr)[-c(1, 2)]) {
      arg <- expr[[k]]
      if (is.name(arg)) {
        if (identical(arg, sym_a)) {
          expr[[k]] <- sym_b
        } else if (identical(arg, sym_b)) {
          expr[[k]] <- sym_a
        }
      }
    }
    return(expr)
  }

  ## Recurse into children
  for (k in seq_len(length(expr) - 1L)) {
    child <- tryCatch(expr[[k + 1L]], error = function(e) NULL)
    if (!is.null(child)) {
      expr[[k + 1L]] <- adjoint_swap_subscript_indices(child, idx_a, idx_b)
    }
  }
  expr
}


adjoint_split_accumulate <- function(expr, name) {
  sym <- as.name(name)
  parts <- monty::monty_differentiation()$maths$as_sum_of_parts(expr)
  is_base <- vlapply(parts, function(p) identical(p, sym))
  if (any(is_base)) {
    base <- sym
    contrib_parts <- parts[!is_base]
    contrib <- monty::monty_differentiation()$maths$plus_fold(contrib_parts)
    list(base = base, contrib = contrib)
  } else {
    list(base = NULL, contrib = expr)
  }
}


generate_array_loops <- function(inner, depends, array, sexp_data, options) {
  res <- inner
  for (idx in rev(array)) {
    if (idx$type == "range") {
      from <- generate_dust_sexp(idx$from, sexp_data, options)
      to <- generate_dust_sexp(idx$to, sexp_data, options)
      size_fns <- c("length", "dim", "OdinDim", "OdinLength")
      if (!rlang::is_call(idx$to, size_fns) && !is.numeric(idx$to)) {
        to <- sprintf("static_cast<size_t>(%s)", to)
      }
      res <- c(sprintf("for (size_t %s = %s; %s <= %s; ++%s) {",
                       idx$name, from, idx$name, to, idx$name),
               res,
               "}")
    } else if (idx$name %in% depends) {
      at <- generate_dust_sexp(idx$at, sexp_data, options)
      res <- c("{",
               sprintf("const size_t %s = %s;", idx$name, at),
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
