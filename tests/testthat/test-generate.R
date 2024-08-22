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


test_that("can generate packing_state function for system of all scalars", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- 1
    initial(y) <- 1
    update(y) <- 1
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_packing_state(dat),
    c(method_args$packing_state,
      '  return dust2::packing{{"x", {}}, {"y", {}}};',
      "}"))
})


test_that("can generate empty packing_gradient function", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- 1
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_packing_gradient(dat),
    c(method_args$packing_gradient,
      '  return dust2::packing{};',
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
  generate_dust_system_update_shared(dat)

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
      "  ll += monty::density::normal(data.d, x, 1, true);",
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
      "  ll += monty::density::normal(data.d, x, a, true);",
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
  packing <- parse_packing(c("x", "y", "b", "z"), NULL)
  dat <- list(storage = list(
                location = c(a = "stack",
                             b = "state",
                             c = "outerspace"),
                type = c("a" = "real", b = "real", c = "real"),
                packing = list(state = packing)))
  expect_equal(
    generate_dust_lhs(list(name = "a"), dat, "mystate"),
    "const real a")
  expect_equal(
    generate_dust_lhs(list(name = "b"), dat, "mystate"),
    "mystate[2]")
  expect_error(
    generate_dust_lhs(list(name = "c"), dat, "mystate"),
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
      "  ll += monty::density::normal(data.d1, x, a, true);",
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
      "  const real_type p = monty::math::exp(x);",
      "  ll += monty::density::poisson(data.d, p, true);",
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
    generate_dust_system_packing_gradient(dat),
    c(method_args$packing_gradient,
      '  return dust2::packing{{"a", {}}};',
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
      "  const real_type p = monty::math::exp(x);",
      "  const real_type adj_p = data.d / p - 1;",
      "  adjoint_next[0] = adj_p * monty::math::exp(x) + adj_x;",
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
      "  state_next[0] = monty::random::normal(rng_state, x, 1);",
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
      "  const auto x = state[0];",
      "  const real_type a = monty::math::log(x);",
      "  const real_type b = monty::math::ceil(a);",
      "  const real_type c = monty::math::pow(a, b);",
      "  state_next[0] = c;",
      "}"))
})


test_that("can generate a simple array equation", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- a[1] + a[2]
    a[] <- Normal(0, 1)
    dim(a) <- 2
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  for (size_t i = 1; i <= 2; ++i) {",
      "    internal.a[i - 1] = monty::random::normal(rng_state, 0, 1);",
      "  }",
      "  state_next[0] = internal.a[0] + internal.a[1];",
      "}"))
  expect_equal(
    generate_dust_system_internal_state(dat),
    c("struct internal_state {",
      "  std::vector<real_type> a;",
      "};"))
  expect_equal(
    generate_dust_system_build_internal(dat),
    c(method_args$build_internal,
      "  std::vector<real_type> a(2);",
      "  return internal_state{a};",
      "}"))
})


test_that("can generate a simple array within shared", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- a[1] + a[2] + a[3]
    a[] <- i
    dim(a) <- 3
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  state_next[0] = shared.a[0] + shared.a[1] + shared.a[2];",
      "}"))

  expect_equal(
    generate_dust_system_shared_state(dat),
    c("struct shared_state {",
      "  std::vector<real_type> a;",
      "};"))
  expect_equal(
    generate_dust_system_build_shared(dat),
    c(method_args$build_shared,
      "  std::vector<real_type> a(3);",
      "  for (size_t i = 1; i <= 3; ++i) {",
      "    a[i - 1] = i;",
      "  }",
      "  return shared_state{a};",
      "}"))

  ## internal state empty despite having arrays:
  expect_equal(
    generate_dust_system_internal_state(dat),
    "struct internal_state {};")
  expect_equal(
    generate_dust_system_build_internal(dat),
    c(method_args$build_internal,
      "  return internal_state{};",
      "}"))
})


test_that("can generate non-range access to arrays", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- a[1]
    a[1] <- Normal(0, 1)
    dim(a) <- 1
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  internal.a[0] = monty::random::normal(rng_state, 0, 1);",
      "  state_next[0] = internal.a[0];",
      "}"))
})


test_that("can generate stochastic initial conditions", {
  dat <- odin_parse({
    a0 <- Poisson(N / 100)
    N <- parameter(1000)
    initial(a) <- a0
    initial(b) <- N - a0
    update(a) <- a
    update(b) <- b
  })
  dat <- generate_prepare(dat)

  expect_equal(
    generate_dust_system_build_shared(dat),
    c(method_args$build_shared,
      "  const real_type N = dust2::r::read_real(parameters, \"N\", 1000);",
      "  return shared_state{N};",
      "}"))

  expect_equal(
    generate_dust_system_initial(dat),
    c(method_args$initial_discrete,
      "  const real_type a0 = monty::random::poisson(rng_state, shared.N / 100);",
      "  state[0] = a0;",
      "  state[1] = shared.N - a0;",
      "}"))
})


test_that("can generate system with array variable", {
  dat <- odin_parse({
    initial(x[]) <- 0
    update(x[]) <- x[i]
    initial(y) <- 0
    update(y) <- y
    dim(x) <- 3
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  const auto * x = state + 0;",
      "  const auto y = state[3];",
      "  for (size_t i = 1; i <= 3; ++i) {",
      "    state_next[i - 1] = x[i - 1];",
      "  }",
      "  state_next[3] = y;",
      "}"))
  expect_equal(
    generate_dust_system_initial(dat),
    c(method_args$initial_discrete,
      "  for (size_t i = 1; i <= 3; ++i) {",
      "    state[i - 1] = 0;",
      "  }",
      "  state[3] = 0;",
      "}"))
  expect_equal(
    generate_dust_system_packing_state(dat),
    c(method_args$packing_state,
      '  return dust2::packing{{"x", {3}}, {"y", {}}};',
      "}"))
})


test_that("can generate system with array variable used in compare", {
  dat <- odin_parse({
    initial(x[]) <- 0
    update(x[]) <- x[i]
    initial(y) <- 0
    update(y) <- y
    dim(x) <- 2
    d <- data()
    compare(d) ~ Normal(x[1] + x[2], y)
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_compare_data(dat),
    c(method_args$compare_data,
      "  const auto * x = state + 0;",
      "  const auto y = state[2];",
      "  real_type ll = 0;",
      "  ll += monty::density::normal(data.d, x[0] + x[1], y, true);",
      "  return ll;",
      "}"))
})


test_that("can generate system with array from user", {
  dat <- odin_parse({
    initial(x) <- 0
    update(x) <- x + a[1] + a[2]
    dim(a) <- 2
    a <- parameter()
  })
  dat <- generate_prepare(dat)

  expect_equal(
    generate_dust_system_build_shared(dat),
    c(method_args$build_shared,
      "  std::vector<real_type> a(2);",
      '  dust2::r::read_real_vector(parameters, 2, a.data(), "a", true);',
      "  return shared_state{a};",
      "}"))
  expect_equal(
    generate_dust_system_update_shared(dat),
    c(method_args$update_shared,
      '  dust2::r::read_real_vector(parameters, 2, shared.a.data(), "a", false);',
      "}"))
})


test_that("can generate system with simple variable sized array", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- a[1] + a[2]
    n <- 2
    a[] <- Normal(0, 1)
    dim(a) <- n
  })

  dat <- generate_prepare(dat)

  expect_equal(
    generate_dust_system_shared_state(dat),
    c("struct shared_state {",
      "  real_type n;",
      "};"))
  expect_equal(
    generate_dust_system_internal_state(dat),
    c("struct internal_state {",
      "  std::vector<real_type> a;",
      "};"))
  expect_equal(
    generate_dust_system_build_shared(dat),
    c(method_args$build_shared,
      "  const real_type n = 2;",
      "  return shared_state{n};",
      "}"))
  expect_equal(
    generate_dust_system_update_shared(dat),
    c(method_args$update_shared,
      "}"))
  expect_equal(
    generate_dust_system_build_internal(dat),
    c(method_args$build_internal,
      "  std::vector<real_type> a(shared.n);",
      "  return internal_state{a};",
      "}"))
  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  for (size_t i = 1; i <= shared.n; ++i) {",
      "    internal.a[i - 1] = monty::random::normal(rng_state, 0, 1);",
      "  }",
      "  state_next[0] = internal.a[0] + internal.a[1];",
      "}"))
})


test_that("can generate system with variable size array that needs saving", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- a[1] + a[2] + a[3]
    n <- 2
    a[] <- Normal(0, 1)
    dim(a) <- n + 1
  })
  dat <- generate_prepare(dat)

  expect_equal(
    generate_dust_system_shared_state(dat),
    c("struct shared_state {",
      "  real_type n;",
      "  size_t dim_a;",
      "};"))
  expect_equal(
    generate_dust_system_internal_state(dat),
    c("struct internal_state {",
      "  std::vector<real_type> a;",
      "};"))
  expect_equal(
    generate_dust_system_build_shared(dat),
    c(method_args$build_shared,
      "  const real_type n = 2;",
      "  const size_t dim_a = n + 1;",
      "  return shared_state{n, dim_a};",
      "}"))
  expect_equal(
    generate_dust_system_update_shared(dat),
    c(method_args$update_shared,
      "}"))
  expect_equal(
    generate_dust_system_build_internal(dat),
    c(method_args$build_internal,
      "  std::vector<real_type> a(shared.dim_a);",
      "  return internal_state{a};",
      "}"))
  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  for (size_t i = 1; i <= shared.dim_a; ++i) {",
      "    internal.a[i - 1] = monty::random::normal(rng_state, 0, 1);",
      "  }",
      "  state_next[0] = internal.a[0] + internal.a[1] + internal.a[2];",
      "}"))
})


test_that("can store arrays in state", {
  ## In this test case we need to generate offsets, which for now
  ## we'll do without generating any special variables
  dat <- odin_parse({
    n <- 2
    initial(a[]) <- 1
    update(a[]) <- a[i] + 1
    dim(a) <- n
    initial(b[]) <- 1
    update(b[]) <- b[i] + 2
    dim(b) <- n + 1
    initial(c[]) <- 1
    update(c[]) <- c[i] + 3
    dim(c) <- n + 2
  })
  dat <- generate_prepare(dat)

  expect_equal(
    generate_dust_system_shared_state(dat),
    c("struct shared_state {",
      "  real_type n;",
      "  size_t dim_b;",
      "  size_t dim_c;",
      "  size_t offset_c;",
      "};"))
  expect_equal(
    generate_dust_system_internal_state(dat),
    "struct internal_state {};")
  expect_equal(
    generate_dust_system_build_shared(dat),
    c(method_args$build_shared,
      "  const real_type n = 2;",
      "  const size_t dim_b = n + 1;",
      "  const size_t dim_c = n + 2;",
      "  const size_t offset_c = n + dim_b;",
      "  return shared_state{n, dim_b, dim_c, offset_c};",
      "}"))
  expect_equal(
    generate_dust_system_update_shared(dat),
    c(method_args$update_shared,
      "}"))
  expect_equal(
    generate_dust_system_build_internal(dat),
    c(method_args$build_internal,
      "  return internal_state{};",
      "}"))
  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  const auto * a = state + 0;",
      "  const auto * b = state + shared.n;",
      "  const auto * c = state + shared.offset_c;",
      "  for (size_t i = 1; i <= shared.n; ++i) {",
      "    state_next[i - 1] = a[i - 1] + 1;",
      "  }",
      "  for (size_t i = 1; i <= shared.dim_b; ++i) {",
      "    state_next[i - 1 + shared.n] = b[i - 1] + 2;",
      "  }",
      "  for (size_t i = 1; i <= shared.dim_c; ++i) {",
      "    state_next[i - 1 + shared.offset_c] = c[i - 1] + 3;",
      "  }",
      "}"))
})


test_that("generate variable sized array from parameters", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- a[1] + a[2]
    n <- 2
    a <- parameter()
    dim(a) <- n
  })

  dat <- generate_prepare(dat)

  expect_equal(
    generate_dust_system_shared_state(dat),
    c("struct shared_state {",
      "  real_type n;",
      "  std::vector<real_type> a;",
      "};"))
  expect_equal(
    generate_dust_system_internal_state(dat),
    "struct internal_state {};")
  expect_equal(
    generate_dust_system_build_shared(dat),
    c(method_args$build_shared,
      "  const real_type n = 2;",
      "  std::vector<real_type> a(n);",
      '  dust2::r::read_real_vector(parameters, n, a.data(), "a", true);',
      "  return shared_state{n, a};",
      "}"))
  expect_equal(
    generate_dust_system_update_shared(dat),
    c(method_args$update_shared,
      "  dust2::r::read_real_vector(parameters, shared.n, shared.a.data(), \"a\", false);",
      "}"))
  expect_equal(
    generate_dust_system_build_internal(dat),
    c(method_args$build_internal,
      "  return internal_state{};",
      "}"))
  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  state_next[0] = shared.a[0] + shared.a[1];",
      "}"))
})
