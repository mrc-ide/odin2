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
      "  std::vector<size_t> odin_index_a;",
      "  odin_index_a.push_back(0);",
      "  const dust2::ode::delay<real_type> a{1, odin_index_a};",
      "  return dust2::ode::delays<real_type>({a});",
      "}"))

  expect_equal(
    generate_dust_system_rhs(dat),
    c(method_args$rhs_delays,
      "  const real_type a = delays[0][0];",
      "  const auto x = state[0];",
      "  state_deriv[0] = x - a;",
      "}"))
})
