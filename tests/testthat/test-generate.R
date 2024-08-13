test_that("generate basic attributes for trivial system", {
  dat <- odin_parse({
    update(x) <- 1
    initial(x) <- 1
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_attributes(dat),
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
    generate_dust_system_shared_state(dat),
    c("struct shared_state {", "};"))
  expect_equal(
    generate_dust_system_internal_state(dat),
    "struct internal_state {};")
  expect_equal(
    generate_dust_system_data_type(dat),
    "using data_type = dust2::no_data;")
})


test_that("don't generate compare functions for systems without them", {
  dat <- odin_parse({
    update(x) <- 1
    initial(x) <- 1
  })
  dat <- generate_prepare(dat)
  expect_null(generate_dust_system_compare_data(dat))
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
    generate_dust_system_attributes(dat),
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
    generate_dust_system_data_type(dat),
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
    generate_dust_system_shared_state(dat),
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
    generate_dust_system_size_state(dat),
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
    generate_dust_system_build_shared(dat),
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
    generate_dust_system_build_shared(dat),
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
  expect_null(generate_dust_system_build_data(dat))
})


test_that("create simple build data function", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- 1
    d <- data()
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_build_data(dat),
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
    generate_dust_system_update_shared(dat),
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
    generate_dust_system_update_shared(dat),
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
    generate_dust_system_build_internal(dat),
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
    generate_dust_system_update_internal(dat),
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
    generate_dust_system_initial(dat),
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
    generate_dust_system_initial(dat),
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
    generate_dust_system_update(dat),
    c(method_args$update,
      "  state_next[0] = 1;",
      "}"))
  expect_null(generate_dust_system_rhs(dat))
})


test_that("can generate simple deriv method", {
  dat <- odin_parse({
    initial(x) <- 1
    deriv(x) <- 1
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_rhs(dat),
    c(method_args$rhs,
      "  state_deriv[0] = 1;",
      "}"))
  expect_null(generate_dust_system_update(dat))
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
    generate_dust_system_compare_data(dat),
    c(method_args$compare_data,
      "  const auto x = state[0];",
      "  real_type ll = 0;",
      "  ll += mcstate::density::normal(data.d, x, 1, true);",
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
    generate_dust_system_compare_data(dat),
    c(method_args$compare_data,
      "  const auto x = state[0];",
      "  real_type ll = 0;",
      "  const real_type a = x / data.d;",
      "  ll += mcstate::density::normal(data.d, x, a, true);",
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
    generate_dust_system_update(dat),
    c(method_args$update,
      "  const auto x = state[0];",
      "  const real_type a = 1 / x;",
      "  state_next[0] = x + a;",
      "}"))
})


test_that("can look up lhs for bits of data", {
  dat <- list(storage = list(
                location = c(a = "stack",
                             b = "state",
                             c = "outerspace"),
                type = c("a" = "real", b = "real", c = "real"),
                packing = list(state = list(scalar = c("x", "y", "b", "z")))))
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


test_that("variables involving data are computed within compare", {
  dat <- odin_parse({
    update(x) <- 1
    initial(x) <- 1
    d1 <- data()
    d2 <- data()
    a <- d1 / d2
    compare(d1) ~ Normal(x, a)
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_compare_data(dat),
    c(method_args$compare_data,
      "  const auto x = state[0];",
      "  real_type ll = 0;",
      "  const real_type a = data.d1 / data.d2;",
      "  ll += mcstate::density::normal(data.d1, x, a, true);",
      "  return ll;",
      "}"))

  ## Check other methods and data structures too
  expect_equal(
    generate_dust_system_data_type(dat),
    c("struct data_type {",
      "  real_type d1;",
      "  real_type d2;",
      "};"))
  expect_equal(
    generate_dust_system_shared_state(dat),
    c("struct shared_state {",
      "};"))
  expect_equal(
    generate_dust_system_build_data(dat),
    c(method_args$build_data,
      '  auto d1 = dust2::r::read_real(data, "d1");',
      '  auto d2 = dust2::r::read_real(data, "d2");',
      '  return data_type{d1, d2};',
      '}'))
  expect_equal(
    generate_dust_system_build_shared(dat),
    c(method_args$build_shared,
      "  return shared_state{};",
      "}"))
})


test_that("pull recursive dependencies into compare_data", {
  dat <- odin_parse({
    update(x) <- 1
    initial(x) <- 1
    p <- exp(x)
    d <- data()
    compare(d) ~ Poisson(p)
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_compare_data(dat),
    c(method_args$compare_data,
      "  const auto x = state[0];",
      "  real_type ll = 0;",
      "  const real_type p = mcstate::math::exp(x);",
      "  ll += mcstate::density::poisson(data.d, p, true);",
      "  return ll;",
      "}"))
})


test_that("generate adjoint", {
  dat <- odin_parse({
    update(x) <- x + a
    initial(x) <- 1
    a <- parameter(differentiate = TRUE)
    p <- exp(x)
    d <- data()
    compare(d) ~ Poisson(p)
  })

  dat <- generate_prepare(dat)

  expect_equal(
    generate_dust_system_adjoint_size(dat),
    c(method_args$adjoint_size,
      "  return 2;",
      "}"))

  expect_equal(
    generate_dust_system_adjoint_update(dat),
    c(method_args$adjoint_update,
      "  const auto adj_x = adjoint[0];",
      "  const auto adj_a = adjoint[1];",
      "  adjoint_next[0] = adj_x;",
      "  adjoint_next[1] = adj_x + adj_a;",
      "}"))

  expect_equal(
    generate_dust_system_adjoint_compare_data(dat),
    c(method_args$adjoint_compare_data,
      "  const auto x = state[0];",
      "  const auto adj_x = adjoint[0];",
      "  const auto adj_a = adjoint[1];",
      "  const real_type p = mcstate::math::exp(x);",
      "  const real_type adj_p = data.d / p - 1;",
      "  adjoint_next[0] = adj_p * mcstate::math::exp(x) + adj_x;",
      "  adjoint_next[1] = adj_a;",
      "}"))

  expect_equal(
    generate_dust_system_adjoint_initial(dat),
    c(method_args$adjoint_initial,
      "  const auto adj_x = adjoint[0];",
      "  const auto adj_a = adjoint[1];",
      "  adjoint_next[0] = adj_x;",
      "  adjoint_next[1] = adj_a;",
      "}"))
})


test_that("can generate simple stochastic system", {
  dat <- odin_parse({
    update(x) <- Normal(x, 1)
    initial(x) <- 0
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  const auto x = state[0];",
      "  state_next[0] = mcstate::random::normal(rng_state, x, 1);",
      "}"))
})


test_that("can generate empty zero_every method", {
  dat <- odin_parse({
    update(x) <- 0
    initial(x) <- 0
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_zero_every(dat),
    c(method_args$zero_every,
      "  return dust2::zero_every_type<real_type>();",
      "}"))
})


test_that("can generate nontrivial zero_every method", {
  dat <- odin_parse({
    update(x) <- 0
    initial(x) <- 0
    update(y) <- 1
    initial(y, zero_every = 4) <- 0
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_zero_every(dat),
    c(method_args$zero_every,
      "  return dust2::zero_every_type<real_type>{{4, {1}}};",
      "}"))
})


test_that("generate defaults for parameters", {
  dat <- odin_parse({
    a <- parameter(2)
    update(x) <- x + a
    initial(x) <- 0
  })

  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_build_shared(dat),
    c(method_args$build_shared,
      '  const real_type a = dust2::r::read_real(parameters, "a", 2);',
      "  return shared_state{a};",
      "}"))
  expect_equal(
    generate_dust_system_update_shared(dat),
    c(method_args$update_shared,
      '  shared.a = dust2::r::read_real(parameters, "a", shared.a);',
      "}"))
})


test_that("can generate models with commonly used mathematical functions", {
  dat <- odin_parse({
    initial(x) <- 0
    update(x) <- c
    a <- log(x)
    b <- ceiling(a)
    c <- a^b
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  const real_type a = mcstate::math::log(x);",
      "  const real_type b = mcstate::math::ceil(a);",
      "  const real_type c = mcstate::math::pow(a, b);",
      "}"))
})
