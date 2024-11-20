test_that("can generate adjoint model", {
  dat <- odin_parse({
    initial(S) <- N - I0
    initial(I) <- I0
    initial(R) <- 0
    initial(cases_inc, zero_every = 1) <- 0
    update(S) <- S - n_SI
    update(I) <- I + n_SI - n_IR
    update(R) <- R + n_IR
    update(cases_inc) <- cases_inc + n_SI
    n_SI <- Binomial(S, p_SI)
    n_IR <- Binomial(I, p_IR)
    p_SI <- 1 - exp(-beta * I / N * dt)
    p_IR <- 1 - exp(-gamma * dt)
    beta <- parameter(differentiate = TRUE)
    gamma <- parameter(differentiate = TRUE)
    I0 <- parameter(differentiate = TRUE)
    N <- parameter(1000)
    exp_noise <- parameter(1e6)
    incidence <- data()
    noise <- Exponential(rate = exp_noise)
    lambda <- cases_inc + noise
    incidence ~ Poisson(lambda)
  })

  dat <- generate_prepare(dat)

  expect_equal(
    generate_dust_system_shared_state(dat),
    c("struct shared_state {",
      "  struct odin_internals_type {",
      "    struct {",
      "      dust2::packing state;",
      "      dust2::packing adjoint;",
      "      dust2::packing gradient;",
      "    } packing;",
      "    struct {",
      "      std::array<size_t, 4> state;",
      "      std::array<size_t, 7> adjoint;",
      "      std::array<size_t, 3> gradient;",
      "    } offset;",
      "  } odin;",
      "  real_type beta;",
      "  real_type gamma;",
      "  real_type I0;",
      "  real_type N;",
      "  real_type exp_noise;",
      "};"))

  expect_equal(
    generate_dust_system_build_shared(dat),
    c(method_args$build_shared,
      '  const real_type beta = dust2::r::read_real(parameters, "beta");',
      '  const real_type gamma = dust2::r::read_real(parameters, "gamma");',
      '  const real_type I0 = dust2::r::read_real(parameters, "I0");',
      '  const real_type N = dust2::r::read_real(parameters, "N", 1000);',
      '  const real_type exp_noise = dust2::r::read_real(parameters, "exp_noise", 1e+06);',
      "  shared_state::odin_internals_type odin;",
      "  odin.packing.state = dust2::packing{",
      '    {"S", {}},',
      '    {"I", {}},',
      '    {"R", {}},',
      '    {"cases_inc", {}}',
      "  };",
      "  odin.packing.adjoint = dust2::packing{",
      '    {"adj_S", {}},',
      '    {"adj_I", {}},',
      '    {"adj_R", {}},',
      '    {"adj_cases_inc", {}},',
      '    {"adj_beta", {}},',
      '    {"adj_gamma", {}},',
      '    {"adj_I0", {}}',
      "  };",
      "  odin.packing.gradient = dust2::packing{",
      '    {"beta", {}},',
      '    {"gamma", {}},',
      '    {"I0", {}}',
      "  };",
      "  odin.packing.state.copy_offset(odin.offset.state.begin());",
      "  odin.packing.adjoint.copy_offset(odin.offset.adjoint.begin());",
      "  odin.packing.gradient.copy_offset(odin.offset.gradient.begin());",
      "  return shared_state{odin, beta, gamma, I0, N, exp_noise};",
      "}"))

  expect_equal(
    generate_dust_system_packing_gradient(dat),
    c(method_args$packing_gradient,
      "  return shared.odin.packing.gradient;",
      "}"))

  expect_equal(
    generate_dust_system_adjoint_update(dat),
    c(method_args$adjoint_update,
      "  const auto S = state[shared.odin.offset.state[0]];",
      "  const auto I = state[shared.odin.offset.state[1]];",
      "  const auto adj_S = adjoint[shared.odin.offset.adjoint[0]];",
      "  const auto adj_I = adjoint[shared.odin.offset.adjoint[1]];",
      "  const auto adj_R = adjoint[shared.odin.offset.adjoint[2]];",
      "  const auto adj_cases_inc = adjoint[shared.odin.offset.adjoint[3]];",
      "  const auto adj_beta = adjoint[shared.odin.offset.adjoint[4]];",
      "  const auto adj_gamma = adjoint[shared.odin.offset.adjoint[5]];",
      "  const auto adj_I0 = adjoint[shared.odin.offset.adjoint[6]];",
      "  const real_type p_SI = 1 - monty::math::exp(-shared.beta * I / shared.N * dt);",
      "  const real_type p_IR = 1 - monty::math::exp(-shared.gamma * dt);",
      "  const real_type adj_n_IR = -adj_I + adj_R;",
      "  const real_type adj_n_SI = -adj_S + adj_I + adj_cases_inc;",
      "  const real_type adj_p_IR = adj_n_IR * I;",
      "  const real_type adj_p_SI = adj_n_SI * S;",
      "  adjoint_next[shared.odin.offset.adjoint[0]] = adj_S + adj_n_SI * p_SI;",
      "  adjoint_next[shared.odin.offset.adjoint[1]] = adj_I + adj_n_IR * p_IR + adj_p_SI * shared.beta * dt * monty::math::exp(-shared.beta * I * dt / shared.N) / shared.N;",
      "  adjoint_next[shared.odin.offset.adjoint[2]] = adj_R;",
      "  adjoint_next[shared.odin.offset.adjoint[3]] = adj_cases_inc;",
      "  adjoint_next[shared.odin.offset.adjoint[4]] = adj_p_SI * I * dt * monty::math::exp(-shared.beta * I * dt / shared.N) / shared.N + adj_beta;",
      "  adjoint_next[shared.odin.offset.adjoint[5]] = adj_p_IR * dt * monty::math::exp(-shared.gamma * dt) + adj_gamma;",
      "  adjoint_next[shared.odin.offset.adjoint[6]] = adj_I0;",
      "}"))

  expect_equal(
    generate_dust_system_adjoint_initial(dat),
    c(method_args$adjoint_initial,
      "  const auto adj_S = adjoint[shared.odin.offset.adjoint[0]];",
      "  const auto adj_I = adjoint[shared.odin.offset.adjoint[1]];",
      "  const auto adj_R = adjoint[shared.odin.offset.adjoint[2]];",
      "  const auto adj_cases_inc = adjoint[shared.odin.offset.adjoint[3]];",
      "  const auto adj_beta = adjoint[shared.odin.offset.adjoint[4]];",
      "  const auto adj_gamma = adjoint[shared.odin.offset.adjoint[5]];",
      "  const auto adj_I0 = adjoint[shared.odin.offset.adjoint[6]];",
      "  adjoint_next[shared.odin.offset.adjoint[0]] = adj_S;",
      "  adjoint_next[shared.odin.offset.adjoint[1]] = adj_I;",
      "  adjoint_next[shared.odin.offset.adjoint[2]] = adj_R;",
      "  adjoint_next[shared.odin.offset.adjoint[3]] = adj_cases_inc;",
      "  adjoint_next[shared.odin.offset.adjoint[4]] = adj_beta;",
      "  adjoint_next[shared.odin.offset.adjoint[5]] = adj_gamma;",
      "  adjoint_next[shared.odin.offset.adjoint[6]] = -adj_S + adj_I + adj_I0;",
      "}"))

  expect_equal(
    generate_dust_system_adjoint_compare_data(dat),
    c(method_args$adjoint_compare_data,
      "  const auto cases_inc = state[shared.odin.offset.state[3]];",
      "  const auto adj_S = adjoint[shared.odin.offset.adjoint[0]];",
      "  const auto adj_I = adjoint[shared.odin.offset.adjoint[1]];",
      "  const auto adj_R = adjoint[shared.odin.offset.adjoint[2]];",
      "  const auto adj_cases_inc = adjoint[shared.odin.offset.adjoint[3]];",
      "  const auto adj_beta = adjoint[shared.odin.offset.adjoint[4]];",
      "  const auto adj_gamma = adjoint[shared.odin.offset.adjoint[5]];",
      "  const auto adj_I0 = adjoint[shared.odin.offset.adjoint[6]];",
      "  const real_type noise = 1 / shared.exp_noise;",
      "  const real_type lambda = cases_inc + noise;",
      "  const real_type adj_lambda = data.incidence / lambda - 1;",
      "  adjoint_next[shared.odin.offset.adjoint[0]] = adj_S;",
      "  adjoint_next[shared.odin.offset.adjoint[1]] = adj_I;",
      "  adjoint_next[shared.odin.offset.adjoint[2]] = adj_R;",
      "  adjoint_next[shared.odin.offset.adjoint[3]] = adj_lambda + adj_cases_inc;",
      "  adjoint_next[shared.odin.offset.adjoint[4]] = adj_beta;",
      "  adjoint_next[shared.odin.offset.adjoint[5]] = adj_gamma;",
      "  adjoint_next[shared.odin.offset.adjoint[6]] = adj_I0;",
      "}"))
})
