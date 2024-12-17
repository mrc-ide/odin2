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
