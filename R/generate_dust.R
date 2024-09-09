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
  dat$sexp_data <- generate_dust_dat(dat$storage$location)
  ## TODO: could merge this above perhaps, these aren't split it in a
  ## very useful place right now.
  if (any(dat$storage$location == "virtual")) {
    virtual <- names(which(dat$storage$location == "virtual"))
    dat$sexp_data$virtual <-
      lapply(dat$equations[virtual], function(x) x$rhs$expr)
  }
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


generate_dust_system_packing_state <- function(dat) {
  generate_dust_system_packing("state", dat)
}


generate_dust_system_packing_gradient <- function(dat) {
  generate_dust_system_packing("gradient", dat)
}


generate_dust_system_packing <- function(name, dat) {
  packing <- dat$storage$packing[[name]]
  args <- c("const shared_state&" = "shared")
  dims <- vcapply(packing$dims, function(el) {
    paste(vcapply(el, generate_dust_sexp, dat$sexp_data), collapse = ", ")
  })
  els <- sprintf('{"%s", {%s}}', packing$name, dims)
  body <- sprintf("return dust2::packing{%s};", paste(els, collapse = ", "))
  cpp_function("dust2::packing", sprintf("packing_%s", name), args, body,
               static = TRUE)
}


generate_dust_system_build_shared <- function(dat) {
  options <- list(shared_exists = FALSE)
  eqs <- setdiff(dat$phases$build_shared$equations,
                 dat$storage$contents$virtual)
  body <- collector()
  for (eq in dat$equations[eqs]) {
    if (eq$lhs$name %in% dat$storage$arrays$name) {
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
    size <- vcapply(dat$storage$arrays$size, generate_dust_sexp, dat$sexp_data)
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
  unpack <- intersect(dat$variables, dat$phases$update$unpack)
  body$add(
    generate_dust_unpack(unpack, dat$storage$packing$state, dat$sexp_data))
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
  unpack <- intersect(dat$variables, dat$phases$deriv$unpack)
  body$add(
    generate_dust_unpack(unpack, dat$storage$packing$state, dat$sexp_data))
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
            "real_type" = "dt",
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

  eqs <- dat$phases$compare$equations
  for (eq in c(dat$equations[eqs])) {
    body$add(generate_dust_assignment(eq, "state", dat))
  }

  ## Then the actual comparison:
  for (eq in dat$phases$compare$compare) {
    eq_args <- vcapply(eq$rhs$args, generate_dust_sexp, dat$sexp_data)
    body$add(sprintf("ll += monty::density::%s(%s, true);",
                     eq$rhs$density$cpp, paste(eq_args, collapse = ", ")))
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
  eqs <- dat$adjoint$update$equations
  for (eq in c(dat$equations[eqs], dat$adjoint$update$adjoint)) {
    body$add(generate_dust_assignment(eq, "adjoint_next", dat))
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

  unpack <- intersect(dat$variables, dat$adjoint$compare$unpack)
  body$add(
    generate_dust_unpack(unpack, dat$storage$packing$state, dat$sexp_data))

  unpack <- intersect(dat$storage$contents$adjoint,
                      dat$adjoint$compare$unpack_adjoint)
  body$add(
    generate_dust_unpack(unpack, dat$storage$packing$adjoint, dat$sexp_data,
                         "adjoint"))

  eqs <- dat$adjoint$compare$equations
  for (eq in c(dat$equations[eqs], dat$adjoint$compare$adjoint)) {
    body$add(generate_dust_assignment(eq, "adjoint_next", dat))
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

  unpack <- intersect(dat$variables, dat$adjoint$initial$unpack)
  body$add(
    generate_dust_unpack(unpack, dat$storage$packing$state, dat$sexp_data))

  unpack <- intersect(dat$storage$contents$adjoint,
                      dat$adjoint$initial$unpack_adjoint)
  body$add(
    generate_dust_unpack(unpack, dat$storage$packing$adjoint, dat$sexp_data,
                         "adjoint"))

  eqs <- dat$adjoint$initial$equations
  for (eq in c(dat$equations[eqs], dat$adjoint$initial$adjoint)) {
    body$add(generate_dust_assignment(eq, "adjoint_next", dat))
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
    packing <- dat$storage$packing$state
    offset <- packing$offset[[match(name, packing$name)]]
    if (is_array) {
      offset <- expr_plus(idx, offset)
    }
    sprintf("%s[%s]",
            name_state,
            generate_dust_sexp(offset, dat$sexp_data, options))
  } else if (location == "adjoint") {
    packing <- dat$storage$packing$adjoint
    offset <- packing$offset[[match(name, packing$name)]]
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
      ## Needs work if more than 1d, some of which is in dust2;
      ## dimensions here as an initialiser list
      stopifnot(dat$storage$arrays$rank[[i]] == 1)
      ## Needs work doing some sort of static initialisation
      stopifnot(is.null(eq$rhs$args$default))
      len <- generate_dust_sexp(dat$storage$arrays$size[[i]],
                                dat$sexp_data, options)
      required <- if (isFALSE(options$shared_exists)) "true" else "false"
      dest <- generate_dust_sexp(name, dat$sexp_data, options)
      res <- sprintf(
        'dust2::r::%s_vector(parameters, %s, %s.data(), "%s", %s);',
        read, len, dest, name, required)
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
  } else {
    is_array <- !is.null(eq$lhs$array)
    lhs <- generate_dust_lhs(eq$lhs, dat, name_state, options)
    rhs <- generate_dust_sexp(eq$rhs$expr, dat$sexp_data, options)

    res <- sprintf("%s = %s;", lhs, rhs)
    is_array <- !is.null(eq$lhs$array)
    if (is_array) {
      for (idx in rev(eq$lhs$array)) {
        if (idx$is_range) {
          from <- generate_dust_sexp(idx$from, dat$sexp_data)
          to <- generate_dust_sexp(idx$to, dat$sexp_data)
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
  offset <- vcapply(packing$offset[i], generate_dust_sexp, sexp_data)
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
