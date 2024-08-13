generate_dust_system <- function(dat) {
  dat <- generate_prepare(dat)

  body <- collector()
  body$add("#include <dust2/common.hpp>")
  body$add(generate_dust_system_attributes(dat))
  body$add(sprintf("class %s {", dat$class))
  body$add("public:")
  body$add(sprintf("  %s() = delete;", dat$class))
  body$add("  using real_type = double;")
  body$add("  using rng_state_type = mcstate::random::generator<real_type>;")
  body$add(sprintf("  %s", generate_dust_system_shared_state(dat)))
  body$add(sprintf("  %s", generate_dust_system_internal_state(dat)))
  body$add(sprintf("  %s", generate_dust_system_data_type(dat)))
  body$add(sprintf("  %s", generate_dust_system_size_state(dat)))
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
  dat$sexp_data <- generate_dust_dat(dat$storage$location)
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
  c("struct shared_state {",
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


generate_dust_system_size_state <- function(dat) {
  args <- c("const shared_state&" = "shared")
  body <- sprintf("return %d;", length(dat$storage$contents$variables))
  cpp_function("size_t", "size_state", args, body, static = TRUE)
}


generate_dust_system_build_shared <- function(dat) {
  options <- list(shared_exists = FALSE)
  eqs <- dat$phases$build_shared$equations
  body <- collector()
  for (eq in dat$equations[eqs]) {
    if (!is.null(eq$lhs$array)) {
      i <- match(eq$lhs$name, dat$storage$arrays$name)
      size <- generate_dust_sexp(dat$storage$arrays$size[[i]], dat$sexp_data,
                                 options)
      body$add(sprintf("std::vector<%s> %s(%s);",
                       dat$storage$type[[eq$lhs$name]], eq$lhs$name, size))
    }
    body$add(generate_dust_assignment(eq, "state", dat, options))
  }
  body$add(sprintf("return shared_state{%s};", paste(eqs, collapse = ", ")))
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
  args <- c("cpp11::list" = "data")
  cpp_function("data_type", "build_data", args, body$get(), static = TRUE)
}


generate_dust_system_build_internal <- function(dat) {
  args <- c("const shared_state&" = "shared")
  if (length(dat$storage$contents$internal) == 0) {
    body <- "return internal_state{};"
  } else {
    nms <- dat$storage$arrays$name
    type <- dat$storage$type[nms]
    size <- vcapply(dat$storage$arrays$size, generate_dust_sexp, dat)
    body <- c(
      sprintf("std::vector<%s> %s(%s);", type, nms, size),
      sprintf("return internal_state{%s};", paste(nms, collapse = ", ")))
  }
  cpp_function("internal_state", "build_internal", args, body, static = TRUE)
}


generate_dust_system_update_shared <- function(dat) {
  eqs <- dat$phases$update_shared$equations
  body <- collector()
  for (eq in dat$equations[eqs]) {
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
              "real_type" = "dt",
              "const shared_state&" = "shared",
              "internal_state&" = "internal",
              "rng_state_type&" = "rng_state",
              "real_type*" = "state")
  }
  body <- collector()
  eqs <- dat$phases$initial$equations
  for (eq in c(dat$equations[eqs], dat$phases$initial$variables)) {
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
  variables <- dat$storage$contents$variables
  i <- variables %in% dat$phases$update$unpack
  body$add(sprintf("const auto %s = state[%d];",
                   variables[i],
                   match(variables[i], dat$storage$packing$state$scalar) - 1))
  eqs <- dat$phases$update$equations
  for (eq in c(dat$equations[eqs], dat$phases$update$variables)) {
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
  variables <- dat$storage$contents$variables
  i <- variables %in% dat$phases$deriv$unpack
  body$add(sprintf("const auto %s = state[%d];",
                   variables[i],
                   match(variables[i], dat$storage$packing$state$scalar) - 1))
  eqs <- dat$phases$deriv$equations
  for (eq in c(dat$equations[eqs], dat$phases$deriv$variables)) {
    body$add(generate_dust_assignment(eq, "state_deriv", dat))
  }
  cpp_function("void", "rhs", args, body$get(), static = TRUE)
}


generate_dust_system_zero_every <- function(dat) {
  args <- c("const shared_state&" = "shared")
  if (is.null(dat$zero_every)) {
    body <- "return dust2::zero_every_type<real_type>();"
  } else {
    index <- match(names(dat$zero_every), dat$variables) - 1
    every <- vcapply(dat$zero_every, generate_dust_sexp, dat$sexp_data,
                     USE.NAMES = FALSE)
    str <- paste(sprintf("{%s, {%s}}", every, index), collapse = ", ")
    body <- sprintf("return dust2::zero_every_type<real_type>{%s};", str)
  }
  cpp_function("auto", "zero_every", args, body, static = TRUE)
}


generate_dust_system_compare_data <- function(dat) {
  if (length(dat$phases$compare) == 0) {
    return(NULL)
  }
  args <- c("real_type" = "time",
            "real_type" = "dt",
            "const real_type*" = "state",
            "const data_type&" = "data",
            "const shared_state&" = "shared",
            "internal_state&" = "internal",
            "rng_state_type&" = "rng_state")
  body <- collector()
  variables <- dat$storage$contents$variables
  i <- variables %in% dat$phases$compare$unpack
  body$add(sprintf("const auto %s = state[%d];",
                   variables[i],
                   match(variables[i], dat$storage$packing$state$scalar) - 1))
  ## TODO collision here in names with 'll'; we might need to prefix
  ## with compare_ perhaps?
  body$add("real_type ll = 0;")

  eqs <- dat$phases$compare$equations
  for (eq in c(dat$equations[eqs])) {
    body$add(generate_dust_assignment(eq, "state", dat))
  }

  ## Then the actual comparison:
  for (eq in dat$phases$compare$compare) {
    eq_args <- vcapply(eq$rhs$args, generate_dust_sexp, dat$sexp_data)
    body$add(sprintf("ll += mcstate::density::%s(%s, true);",
                     eq$rhs$density$cpp, paste(eq_args, collapse = ", ")))
  }

  body$add("return ll;")
  cpp_function("real_type", "compare_data", args, body$get(), static = TRUE)
}


generate_dust_system_adjoint <- function(dat) {
  if (is.null(dat$adjoint)) {
    return(NULL)
  }

  c(generate_dust_system_adjoint_size(dat),
    generate_dust_system_adjoint_update(dat),
    generate_dust_system_adjoint_compare_data(dat),
    generate_dust_system_adjoint_initial(dat))
}


generate_dust_system_adjoint_size <- function(dat) {
  args <- c("const shared_state&" = "shared")
  ## We might return the _difference_ here, still undecided...
  body <- sprintf("return %d;", length(dat$storage$contents$adjoint))
  cpp_function("size_t", "adjoint_size", args, body, static = TRUE)
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

  variables <- dat$storage$contents$variables
  i <- variables %in% dat$adjoint$update$unpack
  body$add(sprintf("const auto %s = state[%d];",
                   variables[i],
                   match(variables[i], dat$storage$packing$state$scalar) - 1))

  adjoint <- dat$storage$contents$adjoint
  i <- adjoint %in% dat$adjoint$update$unpack_adjoint
  body$add(sprintf("const auto %s = adjoint[%d];",
                   adjoint[i],
                   match(adjoint[i], dat$storage$packing$adjoint$scalar) - 1))

  eqs <- dat$adjoint$update$equations
  for (eq in c(dat$equations[eqs], dat$adjoint$update$adjoint)) {
    body$add(generate_dust_assignment(eq, "state_next", dat))
  }

  cpp_function("void", "adjoint_update", args, body$get(), static = TRUE)
}


generate_dust_system_adjoint_compare_data <- function(dat) {
  args <- c("real_type" = "time",
            "real_type" = "dt",
            "const real_type*" = "state",
            "const real_type*" = "adjoint",
            "const data_type&" = "data",
            "const shared_state&" = "shared",
            "internal_state&" = "internal",
            "real_type*" = "adjoint_next")
  body <- collector()

  variables <- dat$storage$contents$variables
  i <- variables %in% dat$adjoint$compare$unpack
  body$add(sprintf("const auto %s = state[%d];",
                   variables[i],
                   match(variables[i], dat$storage$packing$state$scalar) - 1))

  adjoint <- dat$storage$contents$adjoint
  i <- adjoint %in% dat$adjoint$compare$unpack_adjoint
  body$add(sprintf("const auto %s = adjoint[%d];",
                   adjoint[i],
                   match(adjoint[i], dat$storage$packing$adjoint$scalar) - 1))

  eqs <- dat$adjoint$compare$equations
  for (eq in c(dat$equations[eqs], dat$adjoint$compare$adjoint)) {
    body$add(generate_dust_assignment(eq, "state_next", dat))
  }

  cpp_function("void", "adjoint_compare_data", args, body$get(), static = TRUE)
}


generate_dust_system_adjoint_initial <- function(dat) {
  args <- c("real_type" = "time",
            "real_type" = "dt",
            "const real_type*" = "state",
            "const real_type*" = "adjoint",
            "const shared_state&" = "shared",
            "internal_state&" = "internal",
            "real_type*" = "adjoint_next")
  body <- collector()

  variables <- dat$storage$contents$variables
  i <- variables %in% dat$adjoint$initial$unpack
  body$add(sprintf("const auto %s = state[%d];",
                   variables[i],
                   match(variables[i], dat$storage$packing$state$scalar) - 1))

  adjoint <- dat$storage$contents$adjoint
  i <- adjoint %in% dat$adjoint$initial$unpack_adjoint
  body$add(sprintf("const auto %s = adjoint[%d];",
                   adjoint[i],
                   match(adjoint[i], dat$storage$packing$adjoint$scalar) - 1))

  eqs <- dat$adjoint$initial$equations
  for (eq in c(dat$equations[eqs], dat$adjoint$initial$adjoint)) {
    body$add(generate_dust_assignment(eq, "state_next", dat))
  }

  cpp_function("void", "adjoint_initial", args, body$get(), static = TRUE)
}


generate_dust_lhs <- function(lhs, dat, name_state, options) {
  name <- lhs$name
  is_array <- !is.null(lhs$array)
  if (is_array) {
    if (length(lhs$array) > 1) {
      stop("cope with matrix here")
    }
    is_range <- lhs$array[[1]]$is_range
    if (is_range) {
      idx <- expr_minus(as.name(INDEX[[1]]), 1)
    } else {
      idx <- expr_minus(lhs$array[[1]]$at, 1)
    }
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
    if (is_array) {
      stop("support arrays in variables")
    }
    ## TODO: this wil need to change, once we support arrays: at that
    ## point we'll make this more efficient too by computing
    ## expressions for access.
    sprintf("%s[%s]",
            name_state, match(name, dat$storage$packing$state$scalar) - 1)
  } else if (location == "adjoint") {
    if (is_array) {
      stop("support arrays in adjoint")
    }
    sprintf("%s[%s]",
            "adjoint_next", match(name, dat$storage$packing$adjoint$scalar) - 1)
  } else {
    stop(sprintf("Unsupported location '%s'", location))
  }
}


generate_dust_assignment <- function(eq, name_state, dat,
                                     options = list()) {
  lhs <- generate_dust_lhs(eq$lhs, dat, name_state, options)

  if (eq$rhs$type == "parameter") {
    if (isFALSE(options$shared_exists)) {
      if (is.null(eq$rhs$args$default)) {
        rhs <- sprintf('dust2::r::read_real(parameters, "%s")', eq$lhs$name)
      } else {
        default <- generate_dust_sexp(
          eq$rhs$args$default, dat$sexp_data, options)
        rhs <- sprintf('dust2::r::read_real(parameters, "%s", %s)',
                       eq$lhs$name, default)
      }
    } else {
      rhs <- sprintf('dust2::r::read_real(parameters, "%s", %s)',
                     eq$lhs$name, lhs)
    }
  } else {
    rhs <- generate_dust_sexp(eq$rhs$expr, dat$sexp_data, options)
  }
  res <- sprintf("%s = %s;", lhs, rhs)
  is_array <- !is.null(eq$lhs$array)
  if (is_array) {
    for (idx in rev(eq$lhs$array)) {
      if (idx$is_range) {
        from <- generate_dust_sexp(idx$from, dat)
        to <- generate_dust_sexp(idx$to, dat)
        res <- c(sprintf("for (size_t %s = %s; %s < %s; ++%s) {",
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
  res
}
