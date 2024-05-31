generate_dust_model <- function(dat) {
  core <- generate_dust_model_core(dat)
  body <- collector()
  body$add("#include <dust2/common.hpp>")
  body$add(core$attributes)
  body$add(sprintf("class %s {", dat$class))
  body$add("public:")
  body$add(sprintf("  %s() = delete;", dat$class))
  body$add("  using real_type = double;")
  body$add("  using data_type = dust2::no_data;")
  body$add("  using rng_state_type = mcstate::random::generator<real_type>;")
  body$add(sprintf("  %s", core$shared_state))
  body$add(sprintf("  %s", core$internal_state))
  body$add(sprintf("  %s", core$data))
  body$add(sprintf("  %s", core$size))
  body$add(sprintf("  %s", core$build_shared))
  body$add(sprintf("  %s", core$build_internal))
  body$add(sprintf("  %s", core$update_shared))
  body$add(sprintf("  %s", core$update_internal))
  body$add(sprintf("  %s", core$initial))
  body$add(sprintf("  %s", core$update))
  body$add(sprintf("  %s", core$compare_data))
  body$add("};")
  body$get()
}


generate_dust_model_core <- function(dat) {
  list(attributes = generate_dust_model_core_attributes(dat),
       shared_state = generate_dust_model_core_shared_state(dat),
       internal_state = generate_dust_model_core_internal_state(dat),
       size = generate_dust_model_core_size(dat),
       build_shared = generate_dust_model_core_build_shared(dat),
       build_internal = generate_dust_model_core_build_internal(dat),
       update_shared = generate_dust_model_core_update_shared(dat),
       update_internal = generate_dust_model_core_update_internal(dat),
       initial = generate_dust_model_core_initial(dat),
       update = generate_dust_model_core_update(dat),
       compare_data = generate_dust_model_core_compare_data(dat))
}


generate_dust_model_core_attributes <- function(dat) {
  if (length(dat$phases$compare)) {
    has_compare <- "// [[dust2::has_compare()]]"
  } else {
    has_compare <- NULL
  }
  c(sprintf("// [[dust2::class(%s)]]", dat$class),
    has_compare)
}


generate_dust_model_core_shared_state <- function(dat) {
  nms <- dat$location$contents$shared
  type <- dat$location$type[nms]
  c("struct shared_state {",
    sprintf("  %s %s;", type, nms),
    "};")
}


generate_dust_model_core_internal_state <- function(dat) {
  "struct internal_state {};"
}


generate_dust_model_core_data <- function(dat) {
  data <- dat$location$contents$data
  if (length(data) == 0) {
    "  using data_type = dust2::no_data;"
  } else {
    c("struct data_type = {",
      sprintf("  real_type %s;", data),
      "}")
  }
}


generate_dust_model_core_size <- function(dat) {
  args <- c("const shared_state&" = "shared")
  body <- sprintf("return %d;", length(dat$location$contents$variables))
  cpp_function("size_t", "size", args, body, static = TRUE)
}


generate_dust_model_core_build_shared <- function(dat) {
  eqs <- dat$phases$create_shared$equations
  body <- collector()
  for (eq in dat$equations[eqs]) {
    lhs <- eq$lhs$name
    rhs <- generate_dust_sexp(eq$rhs$expr, dat)
    body$add(sprintf("real_type %s = %s;", lhs, rhs))
  }
  body$add(sprintf("return shared_state{%s};", paste(eqs, collapse = ", ")))
  args <- c("cpp11::list" = "parameters")
  cpp_function("shared_state", "build_shared", args, body$get(), static = TRUE)
}


generate_dust_model_core_build_internal <- function(dat) {
  args <- c("const shared_state&" = "shared")
  body <- "return internal_state{};"
  cpp_function("internal_state", "build_internal", args, body, static = TRUE)
}


generate_dust_model_core_update_shared <- function(dat) {
  args <- c("cpp11::list" = "pars", "shared_state&" = "shared")
  body <- character()
  cpp_function("void", "update_shared", args, body, static = TRUE)
}


generate_dust_model_core_update_internal <- function(dat) {
  args <- c("const shared_state&" = "shared", "internal_state&" = "internal")
  body <- character()
  cpp_function("void", "update_internal", args, body, static = TRUE)
}


generate_dust_model_core_initial <- function(dat) {
  args <- c("real_type" = "time",
            "real_type" = "dt",
            "const shared_state&" = "shared",
            "internal_state&" = "internal",
            "rng_state_type&" = "rng_state",
            "real_type*" = "state")
  body <- collector()
  eqs <- dat$phases$initial$equations
  for (eq in c(dat$equations[eqs], dat$phases$initial$variables)) {
    lhs <- generate_dust_lhs(eq$lhs$name, dat, "state")
    rhs <- generate_dust_sexp(eq$rhs$expr, dat)
    body$add(sprintf("%s = %s;", lhs, rhs))
  }
  cpp_function("void", "initial", args, body$get(), static = TRUE)
}


generate_dust_model_core_update <- function(dat) {
  args <- c("real_type" = "time",
            "real_type" = "dt",
            "const real_type*" = "state",
            "const shared_state&" = "shared",
            "internal_state&" = "internal",
            "rng_state_type&" = "rng_state",
            "real_type*" = "state_next")
  body <- collector()
  variables <- dat$location$contents$variables
  packing <- dat$location$packing$state
  i <- variables %in% dat$phases$update$unpack
  ## TODO: this will get changed, and also reused.
  body$add(sprintf("const auto %s = state[%d];",
                   variables[i], unlist(packing[i])))
  eqs <- dat$phases$update$equations
  for (eq in c(dat$equations[eqs], dat$phases$update$variables)) {
    lhs <- generate_dust_lhs(eq$lhs$name, dat, "state_next")
    rhs <- generate_dust_sexp(eq$rhs$expr, dat)
    body$add(sprintf("%s = %s;", lhs, rhs))
  }
  cpp_function("void", "update", args, body$get(), static = TRUE)
}


generate_dust_model_core_compare_data <- function(dat) {
  args <- c("real_type" = "time",
            "real_type" = "dt",
            "const real_type*" = "state",
            "const data_type&" = "data",
            "const shared_state&" = "shared",
            "internal_state&" = "internal",
            "rng_state_type&" = "rng_state")
  body <- collector()
  variables <- dat$location$contents$variables
  packing <- dat$location$packing$state
  i <- variables %in% dat$phases$update$unpack
  ## TODO: this will get changed, and also reused.
  body$add(sprintf("const auto %s = state[%d];",
                   variables[i], unlist(packing[i])))
  body$add("real_type ll = 0;") # TODO collision here

  eqs <- dat$phases$update$equations
  for (eq in c(dat$equations[eqs], dat$phases$update$variables)) {
    rhs <- generate_dust_sexp(eq$rhs$expr, dat)
    body$add(sprintf("ll += %s;", rhs))
  }

  body$add("return ll;")
  cpp_function("real_type", "compare_data", args, body$get(), static = TRUE)
}
