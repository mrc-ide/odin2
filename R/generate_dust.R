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
  body$add("};")
  body$get()
}


generate_prepare <- function(dat) {
  dat$sexp_data <- generate_dust_dat(dat$storage$location)
  dat
}


generate_dust_system_attributes <- function(dat) {
  if (length(dat$phases$compare) > 0) {
    has_compare <- "// [[dust2::has_compare()]]"
  } else {
    has_compare <- NULL
  }
  c(sprintf("// [[dust2::class(%s)]]", dat$class),
    sprintf("// [[dust2::time_type(%s)]]", dat$time),
    has_compare)
}


generate_dust_system_shared_state <- function(dat) {
  nms <- dat$storage$contents$shared
  type <- dat$storage$type[nms]
  c("struct shared_state {",
    sprintf("  %s %s;", type, nms),
    "};")
}


## This one is trivial until we pick up arrays
generate_dust_system_internal_state <- function(dat) {
  "struct internal_state {};"
}


generate_dust_system_data_type <- function(dat) {
  data <- dat$storage$contents$data
  if (nrow(data) == 0) {
    "using data_type = dust2::no_data;"
  } else {
    c("struct data_type {",
      sprintf("  real_type %s;", data$name),
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
    lhs <- eq$lhs$name
    if (eq$rhs$type == "parameter") {
      rhs <- sprintf('dust2::r::read_real(parameters, "%s")', lhs)
    } else {
      rhs <- generate_dust_sexp(eq$rhs$expr, dat$sexp_data, options)
    }
    body$add(sprintf("const real_type %s = %s;", lhs, rhs))
  }
  body$add(sprintf("return shared_state{%s};", paste(eqs, collapse = ", ")))
  args <- c("cpp11::list" = "parameters")
  cpp_function("shared_state", "build_shared", args, body$get(), static = TRUE)
}


generate_dust_system_build_data <- function(dat) {
  data <- dat$storage$contents$data
  if (nrow(data) == 0) {
    return(NULL)
  }
  ## This is very simple for now, but later we can allow data to have
  ## types, lengths, etc.
  eqs <- dat$phases$create_data$equations
  body <- collector()
  body$add(sprintf('auto %s = dust2::r::read_real(data, "%s");',
                   data$name, data$name))
  body$add(sprintf("return data_type{%s};", paste(data, collapse = ", ")))
  args <- c("cpp11::list" = "data")
  cpp_function("data_type", "build_data", args, body$get(), static = TRUE)
}


generate_dust_system_build_internal <- function(dat) {
  args <- c("const shared_state&" = "shared")
  body <- "return internal_state{};"
  cpp_function("internal_state", "build_internal", args, body, static = TRUE)
}


generate_dust_system_update_shared <- function(dat) {
  eqs <- dat$phases$update_shared$equations
  body <- collector()
  for (eq in dat$equations[eqs]) {
    name <- eq$lhs$name
    lhs <- generate_dust_sexp(name, dat$sexp_data)
    if (eq$rhs$type == "parameter") {
      rhs <- sprintf('dust2::r::read_real(parameters, "%s", %s)', name, lhs)
    } else {
      rhs <- generate_dust_sexp(eq$rhs$expr, dat$sexp_data)
    }
    body$add(sprintf("%s = %s;", lhs, rhs))
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
    lhs <- generate_dust_lhs(eq$lhs$name, dat, "state")
    rhs <- generate_dust_sexp(eq$rhs$expr, dat$sexp_data)
    body$add(sprintf("%s = %s;", lhs, rhs))
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
                   match(variables[i], dat$storage$packing$scalar) - 1))
  eqs <- dat$phases$update$equations
  for (eq in c(dat$equations[eqs], dat$phases$update$variables)) {
    lhs <- generate_dust_lhs(eq$lhs$name, dat, "state_next")
    rhs <- generate_dust_sexp(eq$rhs$expr, dat$sexp_data)
    body$add(sprintf("%s = %s;", lhs, rhs))
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
                   match(variables[i], dat$storage$packing$scalar) - 1))
  eqs <- dat$phases$deriv$equations
  for (eq in c(dat$equations[eqs], dat$phases$deriv$variables)) {
    lhs <- generate_dust_lhs(eq$lhs$name, dat, "state_deriv")
    rhs <- generate_dust_sexp(eq$rhs$expr, dat$sexp_data)
    body$add(sprintf("%s = %s;", lhs, rhs))
  }
  cpp_function("void", "rhs", args, body$get(), static = TRUE)
}


generate_dust_system_zero_every <- function(dat) {
  args <- c("const shared_state&" = "shared")
  body <- "return dust2::zero_every_type<real_type>();"
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
                   match(variables[i], dat$storage$packing$scalar) - 1))
  ## TODO collision here in names with 'll'; we might need to prefix
  ## with compare_ perhaps?
  body$add("real_type ll = 0;")

  eqs <- dat$phases$compare$equations
  for (eq in c(dat$equations[eqs])) {
    lhs <- generate_dust_lhs(eq$lhs$name, dat, "state")
    rhs <- generate_dust_sexp(eq$rhs$expr, dat$sexp_data)
    body$add(sprintf("%s = %s;", lhs, rhs))
  }

  ## Then the actual comparison:
  for (eq in dat$phases$compare$compare) {
    eq_args <- vcapply(eq$rhs$args, generate_dust_sexp, dat$sexp_data)
    body$add(sprintf("ll += mcstate2::density::%s(rng, %s, true);",
                     eq$rhs$distribution, paste(eq_args, collapse = ", ")))
  }

  body$add("return ll;")
  cpp_function("real_type", "compare_data", args, body$get(), static = TRUE)
}


generate_dust_lhs <- function(name, dat, name_state) {
  location <- dat$storage$location[[name]]
  if (location == "stack") {
    sprintf("const %s %s", dat$storage$type[[name]], name)
  } else if (location == "state") {
    ## TODO: this wil need to change, once we support arrays: at that
    ## point we'll make this more efficient too by computing
    ## expressions for access.
    sprintf("%s[%s]", name_state, match(name, dat$storage$packing$scalar) - 1)
  } else {
    stop("Unsupported location")
  }
}
