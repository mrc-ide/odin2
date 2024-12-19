test_that("can generate a very simple delay", {
  dat <- odin_parse({
    deriv(x) <- x - a
    initial(x) <- 0
    a <- delay(x, 1)
  })
  dat <- generate_prepare(dat)

  expect_equal(
    generate_dust_system_delays(dat),
    c(method_args$delays,
      "  const dust2::ode::delay<real_type> a(1, {{0, 1}});",
      "  return dust2::ode::delays<real_type>({a});",
      "}"))

  expect_equal(
    generate_dust_system_rhs(dat),
    c(method_args$rhs_delays,
      "  const auto a = delays[0].data[0];",
      "  const auto x = state[0];",
      "  state_deriv[0] = x - a;",
      "}"))
})


test_that("can generate a delayed array", {
  dat <- odin_parse({
    deriv(x[]) <- x[i] - a[i]
    initial(x[]) <- 0
    a <- delay(x, 1)
    dim(x, a) <- 3
  })
  dat <- generate_prepare(dat)

  expect_equal(
    generate_dust_system_delays(dat),
    c(method_args$delays,
      "  const dust2::ode::delay<real_type> a(1, {{0, shared.dim.x.size}});",
      "  return dust2::ode::delays<real_type>({a});",
      "}"))

  expect_equal(
    generate_dust_system_rhs(dat),
    c(method_args$rhs_delays,
      "  const auto& a = delays[0].data;",
      "  const auto * x = state + 0;",
      "  for (size_t i = 1; i <= shared.dim.x.size; ++i) {",
      "    state_deriv[i - 1 + 0] = x[i - 1] - a[i - 1];",
      "  }",
      "}"))
})


test_that("can generate delay in output", {
  dat <- odin_parse({
    deriv(x) <- 1
    initial(x) <- 0
    a <- delay(x, 1)
    output(y) <- x - a
  })

  dat <- generate_prepare(dat)

  expect_equal(
    generate_dust_system_delays(dat),
    c(method_args$delays,
      "  const dust2::ode::delay<real_type> a(1, {{0, 1}});",
      "  return dust2::ode::delays<real_type>({a});",
      "}"))

  expect_equal(
    generate_dust_system_rhs(dat),
    c(method_args$rhs,
      "  state_deriv[0] = 1;",
      "}"))

  expect_equal(
    generate_dust_system_output(dat),
    c(method_args$output_delays,
      "  const auto a = delays[0].data[0];",
      "  const auto x = state[0];",
      "  state[1] = x - a;",
      "}"))
})


test_that("can generate code for complex delay expression", {
  dat <- odin_parse({
    deriv(x) <- 1
    deriv(y) <- 2
    initial(x) <- 0
    initial(y) <- 0
    a <- x + y
    b <- delay(a, 1)
    output(z) <- b
  })

  dat <- generate_prepare(dat)

  expect_equal(
    generate_dust_system_delays(dat),
    c(method_args$delays,
      "  const dust2::ode::delay<real_type> b(1, {{0, 1}, {1, 1}});",
      "  return dust2::ode::delays<real_type>({b});",
      "}"))

  expect_equal(
    generate_dust_system_output(dat),
    c(method_args$output_delays,
      "  real_type b;",
      "  {",
      "    const auto x = delays[0].data[delays[0].offset[0]];",
      "    const auto y = delays[0].data[delays[0].offset[1]];",
      "    const real_type a = x + y;",
      "    b = a;",
      "  }",
      "  state[2] = b;",
      "}"))
})


test_that("can generate code for an expression array", {
  dat <- odin_parse({
    deriv(x) <- 1
    deriv(y[]) <- 2
    initial(x) <- 0
    initial(y[]) <- 0
    a[] <- x + y[i]
    b <- delay(a, 1)
    output(z[]) <- b[i]
    dim(a, b, y, z) <- 3
  })

  dat <- generate_prepare(dat)

  expect_equal(
    generate_dust_system_delays(dat),
    c(method_args$delays,
      "  const dust2::ode::delay<real_type> b(1, {{0, 1}, {1, shared.dim.a.size}});",
      "  return dust2::ode::delays<real_type>({b});",
      "}"))

  expect_equal(
    generate_dust_system_output(dat),
    c(method_args$output_delays,
      "  {",
      "    const auto x = delays[0].data[delays[0].offset[0]];",
      "    const auto* y = delays[0].data.data() + delays[0].offset[1];",
      "    for (size_t i = 1; i <= shared.dim.a.size; ++i) {",
      "      internal.a[i - 1] = x + y[i - 1];",
      "    }",
      "    std::copy_n(internal.a.data(), shared.dim.a.size, internal.b.data());",
      "  }",
      "  for (size_t i = 1; i <= shared.dim.a.size; ++i) {",
      "    state[i - 1 + 4] = internal.b[i - 1];",
      "  }",
      "}"))
})


test_that("can generate a system with two delays", {
  dat <- odin_parse({
    deriv(x) <- a
    deriv(y) <- 2
    initial(x) <- 0
    initial(y) <- 0
    a <- (x + y) / 2
    b <- delay(a, 1)
    c <- delay(a, 2)
    output(z) <- b + c
  })

  dat <- generate_prepare(dat)

  expect_equal(
    generate_dust_system_delays(dat),
    c(method_args$delays,
      "  const dust2::ode::delay<real_type> b(1, {{0, 1}, {1, 1}});",
      "  const dust2::ode::delay<real_type> c(2, {{0, 1}, {1, 1}});",
      "  return dust2::ode::delays<real_type>({b, c});",
      "}"))

  expect_equal(
    generate_dust_system_output(dat),
    c(method_args$output_delays,
      "  real_type b;",
      "  {",
      "    const auto x = delays[0].data[delays[0].offset[0]];",
      "    const auto y = delays[0].data[delays[0].offset[1]];",
      "    const real_type a = (x + y) / 2;",
      "    b = a;",
      "  }",
      "  real_type c;",
      "  {",
      "    const auto x = delays[1].data[delays[1].offset[0]];",
      "    const auto y = delays[1].data[delays[1].offset[1]];",
      "    const real_type a = (x + y) / 2;",
      "    c = a;",
      "  }",
      "  state[2] = b + c;",
      "}"))
})
