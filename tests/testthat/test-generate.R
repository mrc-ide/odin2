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


test_that("generate basic attributes for system with parameters", {
  dat <- odin_parse({
    update(x) <- 1
    initial(x) <- a + b
    a <- parameter(1)
    b <- parameter()
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_attributes(dat),
    c("// [[dust2::class(odin)]]",
      "// [[dust2::time_type(discrete)]]",
      '// [[dust2::parameter(a, type = "real_type", rank = 0, required = FALSE, constant = FALSE)]]',
      '// [[dust2::parameter(b, type = "real_type", rank = 0, required = TRUE, constant = FALSE)]]'))
})


test_that("generate basic attributes for system with a single parameter", {
  dat <- odin_parse({
    update(x) <- 1
    initial(x) <- a
    a <- parameter(1)
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_attributes(dat),
    c("// [[dust2::class(odin)]]",
      "// [[dust2::time_type(discrete)]]",
      '// [[dust2::parameter(a, type = "real_type", rank = 0, required = FALSE, constant = FALSE)]]'))
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
      "  auto d = dust2::r::read_real(data, \"d\", NA_REAL);",
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
      "  real_type odin_ll = 0;",
      "  if (!std::isnan(data.d)) {",
      "    odin_ll += monty::density::normal(data.d, x, 1, true);",
      "  }",
      "  return odin_ll;",
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
      "  real_type odin_ll = 0;",
      "  const real_type a = x / data.d;",
      "  if (!std::isnan(data.d)) {",
      "    odin_ll += monty::density::normal(data.d, x, a, true);",
      "  }",
      "  return odin_ll;",
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
  packing <- parse_packing(c("x", "y", "b", "z"), NULL, NULL, "state")
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
      "  real_type odin_ll = 0;",
      "  const real_type a = data.d1 / data.d2;",
      "  if (!std::isnan(data.d1) && !std::isnan(data.d2)) {",
      "    odin_ll += monty::density::normal(data.d1, x, a, true);",
      "  }",
      "  return odin_ll;",
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
      '  auto d1 = dust2::r::read_real(data, "d1", NA_REAL);',
      '  auto d2 = dust2::r::read_real(data, "d2", NA_REAL);',
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
      "  real_type odin_ll = 0;",
      "  const real_type p = monty::math::exp(x);",
      "  if (!std::isnan(data.d)) {",
      "    odin_ll += monty::density::poisson(data.d, p, true);",
      "  }",
      "  return odin_ll;",
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

  expect_equal(
    generate_dust_system_update_shared(dat),
    c(method_args$update_shared,
      '  shared.a = dust2::r::read_real(parameters, "a", shared.a);',
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
      "  shared_state::dim_type dim;",
      "  dim.a.set({static_cast<size_t>(3)});",
      "  std::vector<real_type> a(dim.a.size);",
      "  for (size_t i = 1; i <= dim.a.size; ++i) {",
      "    a[i - 1] = i;",
      "  }",
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
      "  real_type odin_ll = 0;",
      "  if (!std::isnan(data.d)) {",
      "    odin_ll += monty::density::normal(data.d, x[0] + x[1], y, true);",
      "  }",
      "  return odin_ll;",
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
      "  shared_state::dim_type dim;",
      "  dim.a.set({static_cast<size_t>(2)});",
      "  std::vector<real_type> a(dim.a.size);",
      '  dust2::r::read_real_array(parameters, dim.a, a.data(), "a", true);',
      "  shared_state::offset_type offset;",
      "  offset.state.x = 0;",
      "  return shared_state{dim, offset, a};",
      "}"))
  expect_equal(
    generate_dust_system_update_shared(dat),
    c(method_args$update_shared,
      '  dust2::r::read_real_array(parameters, shared.dim.a, shared.a.data(), "a", false);',
      "}"))
})


test_that("can generate system with aliased array", {
  dat <- odin_parse({
    initial(x) <- 0
    update(x) <- x + a[1] + b[1]
    dim(a) <- 1
    dim(b) <- dim(a)
    a[] <- 1
    b[] <- 2
  })
  dat <- generate_prepare(dat)

  expect_equal(
    generate_dust_system_shared_state(dat),
    c("struct shared_state {",
      "  struct dim_type {",
      "    dust2::array::dimensions<1> a;",
      "  } dim;",
      "  struct offset_type {",
      "    struct {",
      "      size_t x;",
      "    } state;",
      "  } offset;",
      "  std::vector<real_type> a;",
      "  std::vector<real_type> b;",
      "};"))

  expect_equal(
    generate_dust_system_build_shared(dat),
    c(method_args$build_shared,
      "  shared_state::dim_type dim;",
      "  dim.a.set({static_cast<size_t>(1)});",
      "  std::vector<real_type> a(dim.a.size);",
      "  for (size_t i = 1; i <= dim.a.size; ++i) {",
      "    a[i - 1] = 1;",
      "  }",
      "  std::vector<real_type> b(dim.a.size);",
      "  for (size_t i = 1; i <= dim.a.size; ++i) {",
      "    b[i - 1] = 2;",
      "  }",
      "  shared_state::offset_type offset;",
      "  offset.state.x = 0;",
      "  return shared_state{dim, offset, a, b};",
      "}"))
})


test_that("can generate system with length and sum of aliased array", {
  dat <- odin_parse({
    update(x) <- sb + lb + a[1]
    initial(x) <- 0
    dim(a) <- 5
    dim(b) <- dim(a)
    lb <- length(b)
    sb <- sum(b)
    a[] <- 1
    b[] <- 2
  })
  dat <- generate_prepare(dat)

  expect_equal(
    generate_dust_system_shared_state(dat),
    c("struct shared_state {",
      "  struct dim_type {",
      "    dust2::array::dimensions<1> a;",
      "  } dim;",
      "  struct offset_type {",
      "    struct {",
      "      size_t x;",
      "    } state;",
      "  } offset;",
      "  std::vector<real_type> a;",
      "  real_type lb;",
      "  std::vector<real_type> b;",
      "  real_type sb;",
      "};"))

  expect_equal(
    generate_dust_system_build_shared(dat),
    c(method_args$build_shared,
      "  shared_state::dim_type dim;",
      "  dim.a.set({static_cast<size_t>(5)});",
      "  std::vector<real_type> a(dim.a.size);",
      "  for (size_t i = 1; i <= dim.a.size; ++i) {",
      "    a[i - 1] = 1;",
      "  }",
      "  const real_type lb = dim.a.size;",
      "  std::vector<real_type> b(dim.a.size);",
      "  for (size_t i = 1; i <= dim.a.size; ++i) {",
      "    b[i - 1] = 2;",
      "  }",
      "  const real_type sb = dust2::array::sum<real_type>(b.data(), dim.a);",
      "  shared_state::offset_type offset;",
      "  offset.state.x = 0;",
      "  return shared_state{dim, offset, a, lb, b, sb};",
      "}"))
})


test_that("can generate system with 2-d aliased parameterised array", {
  dat <- odin_parse({
    initial(x) <- 0
    update(x) <- x + a[1, 1] + b[1, 1]
    dim(a) <- parameter(rank = 2)
    dim(b) <- dim(a)
    a[, ] <- 1
    b[, ] <- 2
  })
  dat <- generate_prepare(dat)

  expect_equal(
    generate_dust_system_shared_state(dat),
    c("struct shared_state {",
      "  struct dim_type {",
      "    dust2::array::dimensions<2> a;",
      "  } dim;",
      "  struct offset_type {",
      "    struct {",
      "      size_t x;",
      "    } state;",
      "  } offset;",
      "  std::vector<real_type> a;",
      "  std::vector<real_type> b;",
      "};"))

  expect_equal(
    generate_dust_system_build_shared(dat),
    c(method_args$build_shared,
      "  shared_state::dim_type dim;",
      '  dim.a = dust2::r::read_dimensions<2>(parameters, "a");',
      "  std::vector<real_type> a(dim.a.size);",
      "  for (size_t i = 1; i <= dim.a.dim[0]; ++i) {",
      "    for (size_t j = 1; j <= dim.a.dim[1]; ++j) {",
      "      a[i - 1 + (j - 1) * dim.a.mult[1]] = 1;",
      "    }",
      "  }",
      "  std::vector<real_type> b(dim.a.size);",
      "  for (size_t i = 1; i <= dim.a.dim[0]; ++i) {",
      "    for (size_t j = 1; j <= dim.a.dim[1]; ++j) {",
      "      b[i - 1 + (j - 1) * dim.a.mult[1]] = 2;",
      "    }",
      "  }",
      "  shared_state::offset_type offset;",
      "  offset.state.x = 0;",
      "  return shared_state{dim, offset, a, b};",
      "}"))
})


test_that("Can read parameters into var with aliased dimension", {
  dat <- odin_parse({
    update(x[]) <- x[i] + a
    initial(x[]) <- x0[i]

    dim(x) <- n_x
    dim(x0) <- dim(x)
    x0 <- parameter()
    n_x <- parameter()
    a <- parameter()
  })

  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_build_shared(dat),
    c("static shared_state build_shared(cpp11::list parameters) {",
      "  shared_state::dim_type dim;",
      '  const int n_x = dust2::r::read_int(parameters, "n_x");',
      '  const real_type a = dust2::r::read_real(parameters, "a");',
      "  dim.x.set({static_cast<size_t>(n_x)});",
      "  std::vector<real_type> x0(dim.x.size);",
      '  dust2::r::read_real_array(parameters, dim.x, x0.data(), "x0", true);',
      "  shared_state::offset_type offset;",
      "  offset.state.x = 0;",
      "  return shared_state{dim, offset, n_x, a, x0};",
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
      "  shared_state::dim_type dim;",
      "  const real_type n = 2;",
      "  dim.a.set({static_cast<size_t>(n)});",
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
      "  shared_state::dim_type dim;",
      "  const real_type n = 2;",
      "  dim.a.set({static_cast<size_t>(n + 1)});",
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
      "  shared_state::dim_type dim;",
      "  const real_type n = 2;",
      "  dim.a.set({static_cast<size_t>(n)});",
      "  dim.b.set({static_cast<size_t>(n + 1)});",
      "  dim.c.set({static_cast<size_t>(n + 2)});",
      "  dim.d.set({static_cast<size_t>(n + 3)});",
      "  shared_state::offset_type offset;",
      "  offset.state.a = 0;",
      "  offset.state.b = dim.a.size;",
      "  offset.state.c = offset.state.b + dim.b.size;",
      "  offset.state.d = offset.state.c + dim.c.size;",
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
      "  shared_state::dim_type dim;",
      "  const real_type n = 2;",
      "  dim.a.set({static_cast<size_t>(n)});",
      "  std::vector<real_type> a(dim.a.size);",
      '  dust2::r::read_real_array(parameters, dim.a, a.data(), "a", true);',
      "  shared_state::offset_type offset;",
      "  offset.state.x = 0;",
      "  return shared_state{dim, offset, n, a};",
      "}"))
  expect_equal(
    generate_dust_system_update_shared(dat),
    c(method_args$update_shared,
      "  dust2::r::read_real_array(parameters, shared.dim.a, shared.a.data(), \"a\", false);",
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
      "  shared_state::dim_type dim;",
      "  dim.a.set({static_cast<size_t>(2), static_cast<size_t>(3)});",
      "  shared_state::offset_type offset;",
      "  offset.state.x = 0;",
      "  return shared_state{dim, offset};",
      "}"))
  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  for (size_t i = 1; i <= shared.dim.a.dim[0]; ++i) {",
      "    for (size_t j = 1; j <= shared.dim.a.dim[1]; ++j) {",
      "      internal.a[i - 1 + (j - 1) * shared.dim.a.mult[1]] = monty::random::normal<real_type>(rng_state, 0, 1);",
      "    }",
      "  }",
      "  state_next[0] = internal.a[0] / internal.a[1] + internal.a[shared.dim.a.mult[1]] / internal.a[1 + shared.dim.a.mult[1]] + internal.a[2 * shared.dim.a.mult[1]] / internal.a[1 + 2 * shared.dim.a.mult[1]];",
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
    initial(x[, ]) <- 0
    dim(x) <- c(4, 3)
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  const auto * x = state + 0;",
      "  for (size_t i = 1; i <= shared.dim.x.dim[0]; ++i) {",
      "    for (size_t j = 1; j <= shared.dim.x.dim[1]; ++j) {",
      "      state_next[i - 1 + (j - 1) * shared.dim.x.mult[1] + 0] = x[i - 1 + (j - 1) * shared.dim.x.mult[1]] + shared.dim.x.dim[0] / shared.dim.x.dim[1];",
      "    }",
      "  }",
      "}"))
})


test_that("can generate complete sums over arrays", {
  dat <- odin_parse({
    update(x) <- sum(y)
    initial(x) <- 0
    y[] <- Normal(0, 1)
    dim(y) <- 3
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  for (size_t i = 1; i <= shared.dim.y.size; ++i) {",
      "    internal.y[i - 1] = monty::random::normal<real_type>(rng_state, 0, 1);",
      "  }",
      "  state_next[0] = dust2::array::sum<real_type>(internal.y.data(), shared.dim.y);",
      "}"))
})


test_that("can generate partial sums over arrays", {
  dat <- odin_parse({
    update(x[]) <- sum(y[i, ])
    initial(x[]) <- 0
    y[, ] <- Normal(0, 1)
    dim(y) <- c(3, 4)
    dim(x) <- 3
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  for (size_t i = 1; i <= shared.dim.y.dim[0]; ++i) {",
      "    for (size_t j = 1; j <= shared.dim.y.dim[1]; ++j) {",
      "      internal.y[i - 1 + (j - 1) * shared.dim.y.mult[1]] = monty::random::normal<real_type>(rng_state, 0, 1);",
      "    }",
      "  }",
      "  for (size_t i = 1; i <= shared.dim.x.size; ++i) {",
      "    state_next[i - 1 + 0] = dust2::array::sum<real_type>(internal.y.data(), shared.dim.y, {i - 1, i - 1}, {0, shared.dim.y.dim[1] - 1});",
      "  }",
      "}"))
})


test_that("can add interpolation", {
  dat <- odin_parse({
    update(x) <- y
    initial(x) <- 0
    y <- interpolate(at, ay, "constant")
    at <- parameter()
    ay <- parameter()
    dim(at) <- n
    dim(ay) <- n
    n <- 5
  })

  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  const real_type y = shared.interpolate_y.eval(time);",
      "  state_next[0] = y;",
      "}"))

  expect_equal(
    generate_dust_system_shared_state(dat),
    c("struct shared_state {",
      "  struct dim_type {",
      "    dust2::array::dimensions<1> at;",
      "    dust2::array::dimensions<1> ay;",
      "  } dim;",
      "  struct offset_type {",
      "    struct {",
      "      size_t x;",
      "    } state;",
      "  } offset;",
      "  real_type n;",
      "  std::vector<real_type> at;",
      "  std::vector<real_type> ay;",
      "  dust2::interpolate::InterpolateConstant<real_type> interpolate_y;",
      "};"))

  expect_equal(
    generate_dust_system_build_shared(dat),
    c(method_args$build_shared,
      "  shared_state::dim_type dim;",
      "  const real_type n = 5;",
      "  dim.at.set({static_cast<size_t>(n)});",
      "  dim.ay.set({static_cast<size_t>(n)});",
      "  std::vector<real_type> at(dim.at.size);",
      '  dust2::r::read_real_array(parameters, dim.at, at.data(), "at", true);',
      "  std::vector<real_type> ay(dim.ay.size);",
      '  dust2::r::read_real_array(parameters, dim.ay, ay.data(), "ay", true);',
      '  const auto interpolate_y = dust2::interpolate::InterpolateConstant(at, ay, "at", "ay");',
      "  shared_state::offset_type offset;",
      "  offset.state.x = 0;",
      "  return shared_state{dim, offset, n, at, ay, interpolate_y};",
      "}"))
})


test_that("can generate multi-part array", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- b
    b <- a[1] + a[2]
    n <- parameter(type = "integer", constant = TRUE)
    dim(a) <- n
    a[1] <- 1
    a[2] <- Normal(0, 1)
  })

  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  internal.a[0] = 1;",
      "  internal.a[1] = monty::random::normal<real_type>(rng_state, 0, 1);",
      "  const real_type b = internal.a[0] + internal.a[1];",
      "  state_next[0] = b;",
      "}"))
  expect_equal(
    generate_dust_system_build_shared(dat),
    c(method_args$build_shared,
      "  shared_state::dim_type dim;",
      '  const int n = dust2::r::read_int(parameters, "n");',
      "  dim.a.set({static_cast<size_t>(n)});",
      "  shared_state::offset_type offset;",
      "  offset.state.x = 0;",
      "  return shared_state{dim, offset, n};",
      "}"))
})


test_that("can generate multi-part array in variables", {
  dat <- odin_parse({
    initial(x[]) <- 1
    update(x[1]) <- a[1]
    update(x[2]) <- a[2]
    dim(a) <- 2
    a[] <- Normal(0, 1)
    dim(x) <- 2
  })

  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  for (size_t i = 1; i <= shared.dim.a.size; ++i) {",
      "    internal.a[i - 1] = monty::random::normal<real_type>(rng_state, 0, 1);",
      "  }",
      "  state_next[0] = internal.a[0];",
      "  state_next[1 + 0] = internal.a[1];",
      "}"))
})


test_that("can generate self-referential multi-part array", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- x + a[n]
    n <- parameter(type = "integer", constant = TRUE)
    a[1] <- 1
    a[2] <- 1
    a[3:length(a)] <- a[i - 2] + a[i - 1]
    dim(a) <- n
  })

  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_build_shared(dat),
    c(method_args$build_shared,
      "  shared_state::dim_type dim;",
      '  const int n = dust2::r::read_int(parameters, "n");',
      "  dim.a.set({static_cast<size_t>(n)});",
      "  std::vector<real_type> a(dim.a.size);",
      "  a[0] = 1;",
      "  a[1] = 1;",
      "  for (size_t i = 3; i <= dim.a.size; ++i) {",
      "    a[i - 1] = a[i - 2 - 1] + a[i - 1 - 1];",
      "  }",
      "  shared_state::offset_type offset;",
      "  offset.state.x = 0;",
      "  return shared_state{dim, offset, n, a};",
      "}"))
})


test_that("can accept multidimensional array as parameter", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- x + sum(a)
    a <- parameter()
    dim(a) <- c(3, 4, 5)
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_build_shared(dat),
    c(method_args$build_shared,
      "  shared_state::dim_type dim;",
      "  dim.a.set({static_cast<size_t>(3), static_cast<size_t>(4), static_cast<size_t>(5)});",
      "  std::vector<real_type> a(dim.a.size);",
      '  dust2::r::read_real_array(parameters, dim.a, a.data(), "a", true);',
      "  shared_state::offset_type offset;",
      "  offset.state.x = 0;",
      "  return shared_state{dim, offset, a};",
      "}"))
})


test_that("can interpolate arrays", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- x + sum(a)
    a <- interpolate(at, ay, "constant")
    at <- parameter()
    ay <- parameter()
    nt <- 50
    na <- 3
    dim(at) <- nt
    dim(ay) <- c(na, nt)
    dim(a) <- na
  })

  dat <- generate_prepare(dat)

  expect_equal(
    generate_dust_system_shared_state(dat),
    c("struct shared_state {",
      "  struct dim_type {",
      "    dust2::array::dimensions<1> at;",
      "    dust2::array::dimensions<2> ay;",
      "    dust2::array::dimensions<1> a;",
      "  } dim;",
      "  struct offset_type {",
      "    struct {",
      "      size_t x;",
      "    } state;",
      "  } offset;",
      "  real_type nt;",
      "  real_type na;",
      "  std::vector<real_type> at;",
      "  std::vector<real_type> ay;",
      "  dust2::interpolate::InterpolateConstantArray<real_type, 1> interpolate_a;",
      "};"))

  expect_equal(
    generate_dust_system_build_shared(dat),
    c(method_args$build_shared,
      "  shared_state::dim_type dim;",
      "  const real_type nt = 50;",
      "  const real_type na = 3;",
      "  dim.at.set({static_cast<size_t>(nt)});",
      "  dim.ay.set({static_cast<size_t>(na), static_cast<size_t>(nt)});",
      "  dim.a.set({static_cast<size_t>(na)});",
      "  std::vector<real_type> at(dim.at.size);",
      '  dust2::r::read_real_array(parameters, dim.at, at.data(), "at", true);',
      "  std::vector<real_type> ay(dim.ay.size);",
      '  dust2::r::read_real_array(parameters, dim.ay, ay.data(), "ay", true);',
      '  const auto interpolate_a = dust2::interpolate::InterpolateConstantArray<real_type, 1>(at, ay, dim.a, "at", "ay");',
      "  shared_state::offset_type offset;",
      "  offset.state.x = 0;",
      "  return shared_state{dim, offset, nt, na, at, ay, interpolate_a};",
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
      "  const auto x = state[0];",
      "  shared.interpolate_a.eval(time, internal.a);",
      "  state_next[0] = x + dust2::array::sum<real_type>(internal.a.data(), shared.dim.a);",
      "}"))
})


test_that("can generate user-sized arrays", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- x + sum(a)
    a <- parameter()
    dim(a) <- parameter(rank = 1)
  })

  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_shared_state(dat),
    c("struct shared_state {",
      "  struct dim_type {",
      "    dust2::array::dimensions<1> a;",
      "  } dim;",
      "  struct offset_type {",
      "    struct {",
      "      size_t x;",
      "    } state;",
      "  } offset;",
      "  std::vector<real_type> a;",
      "};"))

  expect_equal(
    generate_dust_system_build_shared(dat),
    c(method_args$build_shared,
      "  shared_state::dim_type dim;",
      '  dim.a = dust2::r::read_dimensions<1>(parameters, "a");',
      "  std::vector<real_type> a(dim.a.size);",
      '  dust2::r::read_real_array(parameters, dim.a, a.data(), "a", true);',
      "  shared_state::offset_type offset;",
      "  offset.state.x = 0;",
      "  return shared_state{dim, offset, a};",
      "}"))

  expect_equal(
    generate_dust_system_update_shared(dat),
    c(method_args$update_shared,
      '  dust2::r::read_real_array(parameters, shared.dim.a, shared.a.data(), "a", false);',
      "}"))
})

test_that("can generate print strings", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- x_next
    x_next <- x * 2
    print("x_next: {x_next}")
  })

  dat <- generate_prepare(dat)

  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  const auto x = state[0];",
      "  const real_type x_next = x * 2;",
      "  state_next[0] = x_next;",
      '  Rprintf("[%f] x_next: %f\\n", time, x_next);',
      "}"))
})


test_that("can generate print that requires additional unpack", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- time
    print("x: {x}")
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  state_next[0] = time;",
      "  const auto x = state[0];",
      '  Rprintf("[%f] x: %f\\n", time, x);',
      "}"))
})


test_that("Generate conditional print", {
  dat <- odin_parse({
    initial(x) <- 1
    update(x) <- x_next
    x_next <- x * 2
    print("x_next: {x_next}", when = x > 10)
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  const auto x = state[0];",
      "  const real_type x_next = x * 2;",
      "  state_next[0] = x_next;",
      "  if (x > 10) {",
      '    Rprintf("[%f] x_next: %f\\n", time, x_next);',
      "  }",
      "}"))
})


test_that("support min/max", {
  dat <- odin_parse({
    update(x) <- min(a) + max(b, c)
    initial(x) <- 0
    a[] <- i
    dim(a) <- 10
    b <- 20
    c <- 30
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  state_next[0] = dust2::array::min<real_type>(shared.a.data(), shared.dim.a) + monty::math::max(shared.b, shared.c);",
      "}"))
})


test_that("cast index variables to int when compared to integers", {
  dat <- odin_parse({
    n <- parameter(type = "integer")
    a[] <- if (i == n) 0 else time
    dim(a) <- 3
    update(x) <- a[n]
    initial(x) <- 0
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  for (size_t i = 1; i <= shared.dim.a.size; ++i) {",
      "    internal.a[i - 1] = (static_cast<int>(i) == shared.n ? 0 : time);",
      "  }",
      "  state_next[0] = internal.a[shared.n - 1];",
      "}"))
})


test_that("cast array size to int when compared to integers", {
  dat <- odin_parse({
    n <- parameter(type = "integer")
    a[] <- time
    b <- if (length(a) == n) 0 else 1
    dim(a) <- 3
    update(x) <- a[1] + b
    initial(x) <- 0
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_build_shared(dat),
    c(method_args$build_shared,
      "  shared_state::dim_type dim;",
      '  const int n = dust2::r::read_int(parameters, "n");',
      "  dim.a.set({static_cast<size_t>(3)});",
      "  const real_type b = (static_cast<int>(dim.a.size) == n ? 0 : 1);",
      "  shared_state::offset_type offset;",
      "  offset.state.x = 0;",
      "  return shared_state{dim, offset, n, b};",
      "}"))
})


test_that("can generate browser code", {
  dat <- odin_parse({
    initial(x) <- 0
    update(x) <- b
    a <- x * 2
    b <- a / x
    browser(phase = "update", when = time > 2)
  })
  dat <- generate_prepare(dat)

  expect_equal(
    generate_dust_system_includes(dat),
    c("#include <dust2/common.hpp>",
      "#include <dust2/r/browser.hpp>"))

  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  const auto x = state[0];",
      "  const real_type a = x * 2;",
      "  const real_type b = a / x;",
      "  state_next[0] = b;",
      "  if (time > 2) {",
      "    auto odin_env = dust2::r::browser::create();",
      '    dust2::r::browser::save(time, "time", odin_env);',
      '    dust2::r::browser::save(x, "x", odin_env);',
      '    dust2::r::browser::save(a, "a", odin_env);',
      '    dust2::r::browser::save(b, "b", odin_env);',
      '    dust2::r::browser::enter(odin_env, "update", time);',
      "  }",
      "}"))
})


test_that("can generate nontrivial debug", {
  dat <- odin_parse({
    p_IR <- 1 - exp(-gamma * dt)
    N <- parameter(1000)
    p_SI <- 1 - exp(-(beta * I / N * dt))
    n_SI <- Binomial(S, p_SI)
    n_IR <- Binomial(I, p_IR)
    update(S) <- S - n_SI
    update(I) <- I + n_SI - n_IR
    update(R) <- R + n_IR
    initial(S) <- N - I0
    initial(I) <- I0
    initial(R) <- 0
    beta <- parameter(0.2)
    gamma <- parameter(0.1)
    I0 <- parameter(10)
    browser(phase = "update", when = I < 10 && time > 20)
  })

  dat <- generate_prepare(dat)

  expect_equal(
    generate_dust_system_includes(dat),
    c("#include <dust2/common.hpp>",
      "#include <dust2/r/browser.hpp>"))

  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  const auto S = state[0];",
      "  const auto I = state[1];",
      "  const auto R = state[2];",
      "  const real_type p_IR = 1 - monty::math::exp(-shared.gamma * dt);",
      "  const real_type p_SI = 1 - monty::math::exp(-(shared.beta * I / shared.N * dt));",
      "  const real_type n_SI = monty::random::binomial<real_type>(rng_state, S, p_SI);",
      "  const real_type n_IR = monty::random::binomial<real_type>(rng_state, I, p_IR);",
      "  state_next[0] = S - n_SI;",
      "  state_next[1] = I + n_SI - n_IR;",
      "  state_next[2] = R + n_IR;",
      "  if (I < 10 && time > 20) {",
      "    auto odin_env = dust2::r::browser::create();",
      '    dust2::r::browser::save(time, "time", odin_env);',
      '    dust2::r::browser::save(S, "S", odin_env);',
      '    dust2::r::browser::save(I, "I", odin_env);',
      '    dust2::r::browser::save(R, "R", odin_env);',
      '    dust2::r::browser::save(shared.N, "N", odin_env);',
      '    dust2::r::browser::save(shared.beta, "beta", odin_env);',
      '    dust2::r::browser::save(shared.gamma, "gamma", odin_env);',
      '    dust2::r::browser::save(shared.I0, "I0", odin_env);',
      '    dust2::r::browser::save(p_IR, "p_IR", odin_env);',
      '    dust2::r::browser::save(p_SI, "p_SI", odin_env);',
      '    dust2::r::browser::save(n_SI, "n_SI", odin_env);',
      '    dust2::r::browser::save(n_IR, "n_IR", odin_env);',
      '    dust2::r::browser::enter(odin_env, "update", time);',
      "  }",
      "}"))
})

test_that("can generate pi", {
  dat <- odin_parse({
    initial(x) <- pi
    a <- sin(180 / pi)
    update(x) <- x + a
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_initial(dat),
    c(method_args$initial_discrete,
      "  state[0] = M_PI;",
      "}"))
})

test_that("Array assignment with index on lhs and rhs (1)", {
  ## See (mrc-5894)
  dat <- odin_parse({
    seed_age_band <- as.integer(4)
    n <- parameter(type = "integer", constant = TRUE)
    dim(x) <- n
    initial(y) <- 0
    update(y) <- sum(x)
    x[] <- Poisson(1)
    x[seed_age_band] <- x[i] + 1
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  for (size_t i = 1; i <= shared.dim.x.size; ++i) {",
      "    internal.x[i - 1] = monty::random::poisson<real_type>(rng_state, 1);",
      "  }",
      "  {",
      "    const size_t i = shared.seed_age_band;",
      "    internal.x[shared.seed_age_band - 1] = internal.x[i - 1] + 1;",
      "  }",
      "  state_next[0] = dust2::array::sum<real_type>(internal.x.data(), shared.dim.x);",
      "}")
  )
})

test_that("Array assignment with index on lhs and rhs (1)", {
  ## See (mrc-5894)
  dat <- odin_parse({
    seed_age_band <- as.integer(4)

    n <- parameter(type = "integer", constant = TRUE)
    dim(x) <- n

    initial(y) <- 0
    update(y) <- sum(x)

    lambda <- parameter()
    dim(lambda) <- n

    x[] <- Poisson(lambda[i])
    x[seed_age_band] <- x[i] + 1 + seed_age_band * 0
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  for (size_t i = 1; i <= shared.dim.x.size; ++i) {",
      "    internal.x[i - 1] = monty::random::poisson<real_type>(rng_state, shared.lambda[i - 1]);",
      "  }",
      "  {",
      "    const size_t i = shared.seed_age_band;",
      "    internal.x[shared.seed_age_band - 1] = internal.x[i - 1] + 1 + shared.seed_age_band * 0;",
      "  }",
      "  state_next[0] = dust2::array::sum<real_type>(internal.x.data(), shared.dim.x);",
      "}")
  )
})

test_that("Array assigment lhs: integer index, rhs: i", {
  ## See (mrc-5894)
  dat <- odin_parse({
    n <- parameter(type = "integer", constant = TRUE)
    dim(x) <- n

    initial(y) <- 0
    update(y) <- sum(x)

    x[] <- Poisson(1)
    x[4] <- x[i] + 1
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  for (size_t i = 1; i <= shared.dim.x.size; ++i) {",
      "    internal.x[i - 1] = monty::random::poisson<real_type>(rng_state, 1);",
      "  }",
      "  {",
      "    const size_t i = 4;",
      "    internal.x[3] = internal.x[i - 1] + 1;",
      "  }",
      "  state_next[0] = dust2::array::sum<real_type>(internal.x.data(), shared.dim.x);",
      "}")
  )
})

test_that("Array assigment lhs: expression index, rhs: i", {
  ## See (mrc-5894)
  dat <- odin_parse({
    seed_age_band <- as.integer(4)
    n <- parameter(type = "integer", constant = TRUE)
    dim(x) <- n
    initial(y) <- 0
    update(y) <- sum(x)
    x[] <- Poisson(1)
    x[seed_age_band + 1] <- x[i] + 1
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  for (size_t i = 1; i <= shared.dim.x.size; ++i) {",
      "    internal.x[i - 1] = monty::random::poisson<real_type>(rng_state, 1);",
      "  }",
      "  {",
      "    const size_t i = shared.seed_age_band + 1;",
      "    internal.x[shared.seed_age_band + 1 - 1] = internal.x[i - 1] + 1;",
      "  }",
      "  state_next[0] = dust2::array::sum<real_type>(internal.x.data(), shared.dim.x);",
      "}")
  )
})

test_that("Array assigment lhs: length index, rhs: i", {
  ## See (mrc-5894)
  dat <- odin_parse({
    n <- parameter(type = "integer", constant = TRUE)
    dim(x) <- n
    initial(y) <- 0
    update(y) <- sum(x)
    x[] <- Poisson(1)
    x[length(x)] <- x[i] + 1
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  for (size_t i = 1; i <= shared.dim.x.size; ++i) {",
      "    internal.x[i - 1] = monty::random::poisson<real_type>(rng_state, 1);",
      "  }",
      "  {",
      "    const size_t i = shared.dim.x.size;",
      "    internal.x[shared.dim.x.size - 1] = internal.x[i - 1] + 1;",
      "  }",
      "  state_next[0] = dust2::array::sum<real_type>(internal.x.data(), shared.dim.x);",
      "}")
  )
})


test_that("cast integers to sizes", {
  dat <- odin_parse({
    n <- parameter(5, type = "integer")
    dim(x) <- n
    x[1:n] <- time
    initial(y) <- 0
    update(y) <- x[1]
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_update(dat),
    c(method_args$update,
      "  for (size_t i = 1; i <= static_cast<size_t>(shared.n); ++i) {",
      "    internal.x[i - 1] = time;",
      "  }",
      "  state_next[0] = internal.x[0];",
      "}"))
})


test_that("can generate system with output", {
  dat <- odin_parse({
    initial(x[]) <- 0
    deriv(x[]) <- x * r[i]
    r <- parameter()
    n <- 3
    dim(x) <- n
    dim(r) <- n
    output(tot) <- sum(x)
  })
  dat <- generate_prepare(dat)

  expect_equal(dat$variables, c("x", "tot"))
  expect_equal(dat$output, "tot")
  expect_equal(dat$storage$packing$state$name, c("x", "tot"))

  expect_equal(
    generate_dust_system_initial(dat),
    c(method_args$initial_continuous,
      "  for (size_t i = 1; i <= shared.dim.x.size; ++i) {",
      "    state[i - 1 + 0] = 0;",
      "  }",
      "}"))

  expect_equal(
    generate_dust_system_rhs(dat),
    c(method_args$rhs,
      "  const auto * x = state + 0;",
      "  for (size_t i = 1; i <= shared.dim.x.size; ++i) {",
      "    state_deriv[i - 1 + 0] = x * shared.r[i - 1];",
      "  }",
      "}"))

  expect_equal(
    generate_dust_system_output(dat),
    c(method_args$output,
      "  const auto * x = state + 0;",
      "  state[shared.offset.state.tot] = dust2::array::sum<real_type>(x, shared.dim.x);",
      "}"))

  expect_equal(
    generate_dust_system_size_output(dat),
    c(method_args$size_output,
      "  return 1;",
      "}"))
})


test_that("can generate code for parameter constraints", {
  dat <- odin_parse({
    r <- parameter(min = 2)
    initial(y) <- 0
    update(y) <- y * r
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_build_shared(dat),
    c(method_args$build_shared,
      '  const real_type r = dust2::r::read_real(parameters, "r");',
      '  dust2::r::check_min_scalar<real_type>(r, 2, "r");',
      "  shared_state::offset_type offset;",
      "  offset.state.y = 0;",
      "  return shared_state{offset, r};",
      "}"))
  expect_equal(
    generate_dust_system_update_shared(dat),
    c(method_args$update_shared,
      '  shared.r = dust2::r::read_real(parameters, "r", shared.r);',
      '  dust2::r::check_min_scalar<real_type>(shared.r, 2, "r");',
      "}"))
})


test_that("can generate code for array parameter constraints", {
  dat <- odin_parse({
    r <- parameter(max = 2)
    dim(r) <- 5
    initial(y) <- 0
    update(y) <- y * sum(r)
  })
  dat <- generate_prepare(dat)
  expect_equal(
    generate_dust_system_build_shared(dat),
    c(method_args$build_shared,
      "  shared_state::dim_type dim;",
      "  dim.r.set({static_cast<size_t>(5)});",
      "  std::vector<real_type> r(dim.r.size);",
      '  dust2::r::read_real_array(parameters, dim.r, r.data(), "r", true);',
      '  dust2::r::check_max_array<real_type>(r, 2, "r");',
      "  shared_state::offset_type offset;",
      "  offset.state.y = 0;",
      "  return shared_state{dim, offset, r};",
      "}"))
  expect_equal(
    generate_dust_system_update_shared(dat),
    c(method_args$update_shared,
      '  dust2::r::read_real_array(parameters, shared.dim.r, shared.r.data(), "r", false);',
      '  dust2::r::check_max_array<real_type>(shared.r, 2, "r");',
      "}"))
})
