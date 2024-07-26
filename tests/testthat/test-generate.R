test_that("generate basic attributes for trivial system", {
  dat <- odin_parse({
    update(x) <- 1
    initial(x) <- 1
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_core_attributes(dat),
    c("// [[dust2::class(odin)]]",
      "// [[dust2::time_type(discrete)]]"))
})


test_that("generate trivial types for trivial system", {
  dat <- odin_parse({
    update(x) <- 1
    initial(x) <- 1
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_core_shared_state(dat),
    c("struct shared_state {", "};"))
  expect_equal(
    generate_dust_system_core_internal_state(dat),
    "struct internal_state {};")
  expect_equal(
    generate_dust_system_core_data_type(dat),
    "using data_type = dust2::no_data;")
})


test_that("don't generate compare functions for systems without them", {
  dat <- odin_parse({
    update(x) <- 1
    initial(x) <- 1
  })
  dat <- generate_prepare(dat)
  expect_null(generate_dust_system_core_compare_data(dat))
})


test_that("generate basic attributes for system with compare", {
  dat <- odin_parse({
    update(x) <- 1
    initial(x) <- 1
    d <- data()
    compare(d) ~ Normal(x, 1)
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_core_attributes(dat),
    c("// [[dust2::class(odin)]]",
      "// [[dust2::time_type(discrete)]]",
      "// [[dust2::has_compare()]]"))
})


test_that("generate data type where used", {
  dat <- odin_parse({
    update(x) <- 1
    initial(x) <- 1
    d1 <- data()
    d2 <- data()
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_core_data_type(dat),
    c("struct data_type {",
      "  real_type d1;",
      "  real_type d2;",
      "};"))
})


test_that("generate shared storage where used", {
  dat <- odin_parse({
    a <- 1
    b <- parameter()
    c <- a + b
    initial(x) <- 1
    update(x) <- x + c
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_core_shared_state(dat),
    c("struct shared_state {",
      "  real_type a;",
      "  real_type b;",
      "  real_type c;",
      "};"))
})


test_that("can generate size_state function for system of all scalars", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- 1
    initial(y) <- 1
    update(y) <- 1
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_core_size_state(dat),
    c(method_args$size_state,
      "  return 2;",
      "}"))
})


test_that("can generate build shared for trivial system", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- 1
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_core_build_shared(dat),
    c(method_args$build_shared,
      "  return shared_state{};",
      "}"))
})


test_that("can generate build shared with calculations", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- b
    a <- parameter()
    b <- a * 2
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_core_build_shared(dat),
    c(method_args$build_shared,
      '  const real_type a = dust2::r::read_real(parameters, "a");',
      "  const real_type b = a * 2;",
      "  return shared_state{a, b};",
      "}"))
})


test_that("don't create build data method for system that lacks data", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- 1
  })
  dat <- generate_prepare(dat)
  expect_null(generate_dust_system_core_build_data(dat))
})


test_that("create simple build data function", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- 1
    d <- data()
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_core_build_data(dat),
    c(method_args$build_data,
      "  auto d = dust2::r::read_real(data, \"d\");",
      "  return data_type{d};",
      "}"))
})


test_that("generate trivial update_shared if all parameters constant", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- 1 + p
    p <- parameter(constant = TRUE)
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_core_update_shared(dat),
    c(method_args$update_shared,
      "}"))
})


test_that("generate nontrivial update_shared if some parameters non-constant", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- 1 + p / q + q2
    p <- parameter(constant = TRUE)
    q <- parameter()
    q2 <- q * q
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_core_update_shared(dat),
    c(method_args$update_shared,
      "  shared.q = dust2::r::read_real(parameters, \"q\", shared.q);",
      "  shared.q2 = shared.q * shared.q;",
      "}"))
})


test_that("simple systems generate trivial build_internal function", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- 1
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_core_build_internal(dat),
    c(method_args$build_internal,
      "  return internal_state{};",
      "}"))
})


test_that("simple systems generate trivial update_internal function", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- 1
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_core_update_internal(dat),
    c(method_args$update_internal,
      "}"))
})


test_that("generate trivial discrete time initial conditions", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- 1
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_core_initial(dat),
    c(method_args$initial_discrete,
      "  state[0] = 1;",
      "}"))
})


test_that("generate trivial continuous time initial conditions", {
  dat <- odin_parse({
    initial(x) <- 1
    deriv(x) <- 1
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_core_initial(dat),
    c(method_args$initial_continuous,
      "  state[0] = 1;",
      "}"))
})


test_that("can generate simple update method", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- 1
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_core_update(dat),
    c(method_args$update,
      "  state_next[0] = 1;",
      "}"))
  expect_null(generate_dust_system_core_rhs(dat))
})


test_that("can generate simple deriv method", {
  dat <- odin_parse({
    initial(x) <- 1
    deriv(x) <- 1
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_core_rhs(dat),
    c(method_args$rhs,
      "  state_deriv[0] = 1;",
      "}"))
  expect_null(generate_dust_system_core_update(dat))
})


test_that("can build simple compare function", {
  dat <- odin_parse({
    update(x) <- 1
    initial(x) <- 1
    d <- data()
    compare(d) ~ Normal(x, 1)
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_core_compare_data(dat),
    c(method_args$compare_data,
      "  const auto x = state[0];",
      "  real_type ll = 0;",
      "  ll += mcstate2::density::normal(rng, shared.d, x, 1, true);",
      "  return ll;",
      "}"))
})


test_that("can build more complex compare function", {
  dat <- odin_parse({
    update(x) <- 1
    initial(x) <- 1
    a <- x / d
    d <- data()
    compare(d) ~ Normal(x, a)
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_core_compare_data(dat),
    c(method_args$compare_data,
      "  const auto x = state[0];",
      "  real_type ll = 0;",
      "  const real_type a = x / shared.d;",
      "  ll += mcstate2::density::normal(rng, shared.d, x, a, true);",
      "  return ll;",
      "}"))
})


test_that("generate stack equations during update", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- x + a
    a <- 1 / x
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_core_update(dat),
    c(method_args$update,
      "  const auto x = state[0];",
      "  const real_type a = 1 / x;",
      "  state_next[0] = x + a;",
      "}"))
})


test_that("can look up lhs for bits of data", {
  dat <- list(storage = list(location = c(a = "stack",
                                          b = "state",
                                          c = "outerspace"),
                             type = c("a" = "real", b = "real", c = "real"),
                             packing = list(scalar = c("x", "y", "b", "z"))))
  expect_equal(
    generate_dust_lhs("a", dat, "mystate"),
    "const real a")
  expect_equal(
    generate_dust_lhs("b", dat, "mystate"),
    "mystate[2]")
  expect_error(
    generate_dust_lhs("c", dat, "mystate"),
    "Unsupported location")
})
