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
    c("struct shared_state {",
      "  struct offset_type {",
      "    struct {",
      "      size_t x;",
      "    } state;",
      "  } offset;",
      "};"))
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
    d ~ Normal(x, 1)
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
      "  struct offset_type {",
      "    struct {",
      "      size_t x;",
      "    } state;",
      "  } offset;",
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
      "  return dust2::packing{",
      '    {"x", {}},',
      '    {"y", {}}',
      "  };",
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
      "  return dust2::packing{",
      "  };",
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
      "  shared_state::offset_type offset;",
      "  offset.state.x = 0;",
      "  return shared_state{offset};",
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
      "  shared_state::offset_type offset;",
      "  offset.state.x = 0;",
      "  return shared_state{offset, a, b};",
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
    d ~ Normal(x, 1)
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
    d ~ Normal(x, a)
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
  packing <- parse_packing(c("x", "y", "b", "z"), NULL, "state")
  dat <- list(storage = list(
                location = c(a = "stack",
                             b = "state",
                             c = "outerspace"),
                type = c("a" = "real", b = "real", c = "real"),
                packing = list(state = packing)))
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_lhs(list(name = "a"), dat, "state", list()),
    "const real a")
  expect_equal(
    generate_dust_lhs(list(name = "b"), dat, "state", list()),
    "state[2]")
  expect_error(
    generate_dust_lhs(list(name = "c"), dat, "state", list()),
    "Unsupported location")
})


test_that("variables involving data are computed within compare", {
  dat <- odin_parse({
    update(x) <- 1
    initial(x) <- 1
    d1 <- data()
    d2 <- data()
    a <- d1 / d2
    d1 ~ Normal(x, a)
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
      test_struct_offset("x"),
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
      "  shared_state::offset_type offset;",
      "  offset.state.x = 0;",
      "  return shared_state{offset};",
      "}"))
})


test_that("pull recursive dependencies into compare_data", {
  dat <- odin_parse({
    update(x) <- 1
    initial(x) <- 1
    p <- exp(x)
    d <- data()
    d ~ Poisson(p)
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
    d ~ Poisson(p)
  })

  dat <- generate_prepare(dat)

  expect_equal(
    generate_dust_system_packing_gradient(dat),
    c(method_args$packing_gradient,
      "  return dust2::packing{",
      '    {"a", {}}',
      "  };",
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
      "  state_next[0] = monty::random::normal<real_type>(rng_state, x, 1);",
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


test_that("can generate duplicated zero_every method", {
  dat <- odin_parse({
    update(x) <- 0
    update(y) <- 1
    initial(x, zero_every = 4) <- 0
    initial(y, zero_every = 4) <- 0
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_zero_every(dat),
    c(method_args$zero_every,
      "  return dust2::zero_every_type<real_type>{{4, {0}}, {4, {1}}};",
      "}"))
})


test_that("can generate duplicated zero_every method with different freq", {
  dat <- odin_parse({
    update(x) <- 0
    update(y) <- 1
    initial(x, zero_every = 4) <- 0
    initial(y, zero_every = 3) <- 0
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_zero_every(dat),
    c(method_args$zero_every,
      "  return dust2::zero_every_type<real_type>{{4, {0}}, {3, {1}}};",
      "}"))
})


test_that("can generate zero_every for array variable", {
  dat <- odin_parse({
    update(x[]) <- x[i] + 1
    initial(x[], zero_every = 4) <- 0
    dim(x) <- 4
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_zero_every(dat),
    c(method_args$zero_every,
      "  std::vector<size_t> zero_every_x;",
      "  for (size_t i = 0; i < 4; ++i) {",
      "    zero_every_x.push_back(i);",
      "  }",
      "  return dust2::zero_every_type<real_type>{{4, zero_every_x}};",
      "}"))
})


test_that("can generate zero_every for array variable with scalar", {
  dat <- odin_parse({
    update(x[]) <- x[i] + 1
    initial(x[], zero_every = 4) <- 0
    update(y) <- y + 1
    initial(y, zero_every = 2) <- 0
    dim(x) <- 4
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_zero_every(dat),
    c(method_args$zero_every,
      "  std::vector<size_t> zero_every_x;",
      "  for (size_t i = 0; i < 4; ++i) {",
      "    zero_every_x.push_back(i);",
      "  }",
      "  return dust2::zero_every_type<real_type>{{4, zero_every_x}, {2, {4}}};",
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
      "  shared_state::offset_type offset;",
      "  offset.state.x = 0;",
      "  return shared_state{offset, a};",
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
      "  for (size_t i = 1; i <= shared.dim.a.size; ++i) {",
      "    internal.a[i - 1] = monty::random::normal<real_type>(rng_state, 0, 1);",
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
      "  std::vector<real_type> a(shared.dim.a.size);",
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
      "  struct dim_type {",
      "    dust2::array::dimensions<1> a;",
      "  } dim;",
      test_struct_offset("x"),
      "  std::vector<real_type> a;",
      "};"))
  expect_equal(
    generate_dust_system_build_shared(dat),
    c(method_args$build_shared,
      "  const dust2::array::dimensions<1> dim_a{static_cast<size_t>(3)};",
      "  std::vector<real_type> a(dim_a.size);",
      "  for (size_t i = 1; i <= dim_a.size; ++i) {",
      "    a[i - 1] = i;",
      "  }",
      "  const shared_state::dim_type dim{dim_a};",
      "  shared_state::offset_type offset;",
      "  offset.state.x = 0;",
      "  return shared_state{dim, offset, a};",
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
      "  internal.a[0] = monty::random::normal<real_type>(rng_state, 0, 1);",
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
      "  shared_state::offset_type offset;",
      "  offset.state.a = 0;",
      "  offset.state.b = 1;",
      "  return shared_state{offset, N};",
      "}"))

  expect_equal(
    generate_dust_system_initial(dat),
    c(method_args$initial_discrete,
      "  const real_type a0 = monty::random::poisson<real_type>(rng_state, shared.N / 100);",
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
      "  for (size_t i = 1; i <= shared.dim.x.size; ++i) {",
      "    state_next[i - 1 + 0] = x[i - 1];",
      "  }",
      "  state_next[3] = y;",
      "}"))
  expect_equal(
    generate_dust_system_initial(dat),
    c(method_args$initial_discrete,
      "  for (size_t i = 1; i <= shared.dim.x.size; ++i) {",
      "    state[i - 1 + 0] = 0;",
      "  }",
      "  state[3] = 0;",
      "}"))
  expect_equal(
    generate_dust_system_packing_state(dat),
    c(method_args$packing_state,
      "  return dust2::packing{",
      '    {"x", std::vector<size_t>(shared.dim.x.dim.begin(), shared.dim.x.dim.end())},',
      '    {"y", {}}',
      "  };",
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
    d ~ Normal(x[1] + x[2], y)
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
      "  const dust2::array::dimensions<1> dim_a{static_cast<size_t>(2)};",
      "  std::vector<real_type> a(dim_a.size);",
      '  dust2::r::read_real_vector(parameters, dim_a.size, a.data(), "a", true);',
      "  const shared_state::dim_type dim{dim_a};",
      "  shared_state::offset_type offset;",
      "  offset.state.x = 0;",
      "  return shared_state{dim, offset, a};",
      "}"))
  expect_equal(
    generate_dust_system_update_shared(dat),
    c(method_args$update_shared,
      '  dust2::r::read_real_vector(parameters, shared.dim.a.size, shared.a.data(), "a", false);',
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
      "  struct dim_type {",
      "    dust2::array::dimensions<1> a;",
      "  } dim;",
      test_struct_offset("x"),
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
      "  const dust2::array::dimensions<1> dim_a{static_cast<size_t>(n)};",
      "  const shared_state::dim_type dim{dim_a};",
      "  shared_state::offset_type offset;",
      "  offset.state.x = 0;",
      "  return shared_state{dim, offset, n};",
      "}"))
  expect_equal(
    generate_dust_system_update_shared(dat),
    c(method_args$update_shared,
      "}"))
  expect_equal(
    generate_dust_system_build_internal(dat),
    c(method_args$build_internal,
      "  std::vector<real_type> a(shared.dim.a.size);",
      "  return internal_state{a};",
      "}"))
  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  for (size_t i = 1; i <= shared.dim.a.size; ++i) {",
      "    internal.a[i - 1] = monty::random::normal<real_type>(rng_state, 0, 1);",
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
      "  struct dim_type {",
      "    dust2::array::dimensions<1> a;",
      "  } dim;",
      test_struct_offset("x"),
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
      "  const dust2::array::dimensions<1> dim_a{static_cast<size_t>(n + 1)};",
      "  const shared_state::dim_type dim{dim_a};",
      "  shared_state::offset_type offset;",
      "  offset.state.x = 0;",
      "  return shared_state{dim, offset, n};",
      "}"))
  expect_equal(
    generate_dust_system_update_shared(dat),
    c(method_args$update_shared,
      "}"))
  expect_equal(
    generate_dust_system_build_internal(dat),
    c(method_args$build_internal,
      "  std::vector<real_type> a(shared.dim.a.size);",
      "  return internal_state{a};",
      "}"))
  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  for (size_t i = 1; i <= shared.dim.a.size; ++i) {",
      "    internal.a[i - 1] = monty::random::normal<real_type>(rng_state, 0, 1);",
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
    initial(d[]) <- 1
    update(d[]) <- d[i] + 4
    dim(d) <- n + 3
  })
  dat <- generate_prepare(dat)

  expect_equal(
    generate_dust_system_shared_state(dat),
    c("struct shared_state {",
      "  struct dim_type {",
      "    dust2::array::dimensions<1> a;",
      "    dust2::array::dimensions<1> b;",
      "    dust2::array::dimensions<1> c;",
      "    dust2::array::dimensions<1> d;",
      "  } dim;",
      "  struct offset_type {",
      "    struct {",
      "      size_t a;",
      "      size_t b;",
      "      size_t c;",
      "      size_t d;",
      "    } state;",
      "  } offset;",
      "  real_type n;",
      "};"))
  expect_equal(
    generate_dust_system_internal_state(dat),
    "struct internal_state {};")
  expect_equal(
    generate_dust_system_build_shared(dat),
    c(method_args$build_shared,
      "  const real_type n = 2;",
      "  const dust2::array::dimensions<1> dim_a{static_cast<size_t>(n)};",
      "  const dust2::array::dimensions<1> dim_b{static_cast<size_t>(n + 1)};",
      "  const dust2::array::dimensions<1> dim_c{static_cast<size_t>(n + 2)};",
      "  const dust2::array::dimensions<1> dim_d{static_cast<size_t>(n + 3)};",
      "  const shared_state::dim_type dim{dim_a, dim_b, dim_c, dim_d};",
      "  shared_state::offset_type offset;",
      "  offset.state.a = 0;",
      "  offset.state.b = dim_a.size;",
      "  offset.state.c = offset.state.b + dim_b.size;",
      "  offset.state.d = offset.state.c + dim_c.size;",
      "  return shared_state{dim, offset, n};",
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
      "  const auto * b = state + shared.offset.state.b;",
      "  const auto * c = state + shared.offset.state.c;",
      "  const auto * d = state + shared.offset.state.d;",
      "  for (size_t i = 1; i <= shared.dim.a.size; ++i) {",
      "    state_next[i - 1 + 0] = a[i - 1] + 1;",
      "  }",
      "  for (size_t i = 1; i <= shared.dim.b.size; ++i) {",
      "    state_next[i - 1 + shared.offset.state.b] = b[i - 1] + 2;",
      "  }",
      "  for (size_t i = 1; i <= shared.dim.c.size; ++i) {",
      "    state_next[i - 1 + shared.offset.state.c] = c[i - 1] + 3;",
      "  }",
      "  for (size_t i = 1; i <= shared.dim.d.size; ++i) {",
      "    state_next[i - 1 + shared.offset.state.d] = d[i - 1] + 4;",
      "  }",
      "}"))
  expect_equal(
    generate_dust_system_packing_state(dat),
    c(method_args$packing_state,
      "  return dust2::packing{",
      '    {"a", std::vector<size_t>(shared.dim.a.dim.begin(), shared.dim.a.dim.end())},',
      '    {"b", std::vector<size_t>(shared.dim.b.dim.begin(), shared.dim.b.dim.end())},',
      '    {"c", std::vector<size_t>(shared.dim.c.dim.begin(), shared.dim.c.dim.end())},',
      '    {"d", std::vector<size_t>(shared.dim.d.dim.begin(), shared.dim.d.dim.end())}',
      "  };",
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
      "  struct dim_type {",
      "    dust2::array::dimensions<1> a;",
      "  } dim;",
      test_struct_offset("x"),
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
      "  const dust2::array::dimensions<1> dim_a{static_cast<size_t>(n)};",
      "  std::vector<real_type> a(dim_a.size);",
      '  dust2::r::read_real_vector(parameters, dim_a.size, a.data(), "a", true);',
      "  const shared_state::dim_type dim{dim_a};",
      "  shared_state::offset_type offset;",
      "  offset.state.x = 0;",
      "  return shared_state{dim, offset, n, a};",
      "}"))
  expect_equal(
    generate_dust_system_update_shared(dat),
    c(method_args$update_shared,
      "  dust2::r::read_real_vector(parameters, shared.dim.a.size, shared.a.data(), \"a\", false);",
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


test_that("can include integer parameters", {
  dat <- odin_parse({
    a <- parameter(type = "integer")
    b <- parameter(type = "logical")
    c <- parameter(type = "real")
    initial(x) <- 1
    update(x) <- if (b) a else c
  })
  dat <- generate_prepare(dat)

  expect_equal(
    generate_dust_system_shared_state(dat),
    c("struct shared_state {",
      test_struct_offset("x"),
      "  int a;",
      "  bool b;",
      "  real_type c;",
      "};"))

  expect_equal(
    generate_dust_system_build_shared(dat),
    c(method_args$build_shared,
      '  const int a = dust2::r::read_int(parameters, "a");',
      '  const bool b = dust2::r::read_bool(parameters, "b");',
      '  const real_type c = dust2::r::read_real(parameters, "c");',
      "  shared_state::offset_type offset;",
      "  offset.state.x = 0;",
      "  return shared_state{offset, a, b, c};",
      "}"))
})


test_that("can generate a simple multidimensional array equation", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- a[1, 1] / a[2, 1] + a[1, 2] / a[2, 2] + a[1, 3] / a[2, 3]
    a[, ] <- Normal(0, 1)
    dim(a) <- c(2, 3)
  })

  dat <- generate_prepare(dat)

  expect_equal(
    generate_dust_system_shared_state(dat),
    c("struct shared_state {",
    "  struct dim_type {",
    "    dust2::array::dimensions<2> a;",
    "  } dim;",
    test_struct_offset("x"),
    "};"))
  expect_equal(
    generate_dust_system_build_shared(dat),
    c(method_args$build_shared,
      "  const dust2::array::dimensions<2> dim_a{static_cast<size_t>(2), static_cast<size_t>(3)};",
      "  const shared_state::dim_type dim{dim_a};",
      "  shared_state::offset_type offset;",
      "  offset.state.x = 0;",
      "  return shared_state{dim, offset};",
      "}"))
  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  for (size_t i = 1; i <= shared.dim.a.dim[0]; ++i) {",
      "    for (size_t j = 1; j <= shared.dim.a.dim[1]; ++j) {",
      "      internal.a[i - 1 + (j - 1) * (shared.dim.a.mult[1])] = monty::random::normal<real_type>(rng_state, 0, 1);",
      "    }",
      "  }",
      "  state_next[0] = internal.a[0] / internal.a[1] + internal.a[shared.dim.a.mult[1]] / internal.a[1 + shared.dim.a.mult[1]] + internal.a[2 * (shared.dim.a.mult[1])] / internal.a[1 + 2 * (shared.dim.a.mult[1])];",
      "}"))
})


test_that("can use length() on the rhs", {
  dat <- odin_parse({
    update(x[]) <- x[i] + length(x)
    initial(x[]) <- 0
    dim(x) <- 4
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  const auto * x = state + 0;",
      "  for (size_t i = 1; i <= shared.dim.x.size; ++i) {",
      "    state_next[i - 1 + 0] = x[i - 1] + shared.dim.x.size;",
      "  }",
      "}"))
})


test_that("can use nrow() and ncol() on the rhs", {
  dat <- odin_parse({
    update(x[, ]) <- x[i, j] + nrow(x) / ncol(x)
    initial(x[]) <- 0
    dim(x) <- c(4, 3)
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  const auto * x = state + 0;",
      "  for (size_t i = 1; i <= shared.dim.x.dim[0]; ++i) {",
      "    for (size_t j = 1; j <= shared.dim.x.dim[1]; ++j) {",
      "      state_next[i - 1 + (j - 1) * (shared.dim.x.mult[1]) + 0] = x[i - 1 + (j - 1) * (shared.dim.x.mult[1])] + shared.dim.x.dim[0] / shared.dim.x.dim[1];",
      "    }",
      "  }",
      "}"))
})


test_that("can generate sums over arrays", {

  dat <- odin_parse({
    update(x) <- sum(y)
    initial(x) <- 0
    y[] <- Normal(0, 1)
    dim(y) <- 3
  })
  dat <- generate_prepare(dat)
  generate_dust_system_update(dat)

  dat <- odin_parse({
    update(x[]) <- sum(y[i, ])
    initial(x[]) <- 0
    y[, ] <- Normal(0, 1)
    dim(y) <- c(3, 4)
    dim(x) <- 3
  })
  dat <- generate_prepare(dat)
  generate_dust_system_update(dat)
})
