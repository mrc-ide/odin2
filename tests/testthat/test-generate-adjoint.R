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
    generate_dust_system_packing_gradient(dat),
    c(method_args$packing_gradient,
      "  return dust2::packing{",
      '    {"beta", {}},',
      '    {"gamma", {}},',
      '    {"I0", {}}',
      "  };",
      "}"))

  expect_equal(
    generate_dust_system_adjoint_update(dat),
    c(method_args$adjoint_update,
      "  const auto S = state[0];",
      "  const auto I = state[1];",
      "  const auto adj_S = adjoint[0];",
      "  const auto adj_I = adjoint[1];",
      "  const auto adj_R = adjoint[2];",
      "  const auto adj_cases_inc = adjoint[3];",
      "  const auto adj_beta = adjoint[4];",
      "  const auto adj_gamma = adjoint[5];",
      "  const auto adj_I0 = adjoint[6];",
      "  const real_type p_SI = 1 - monty::math::exp(-shared.beta * I / shared.N * dt);",
      "  const real_type p_IR = 1 - monty::math::exp(-shared.gamma * dt);",
      "  const real_type adj_n_IR = -adj_I + adj_R;",
      "  const real_type adj_n_SI = -adj_S + adj_I + adj_cases_inc;",
      "  const real_type adj_p_IR = adj_n_IR * I;",
      "  const real_type adj_p_SI = adj_n_SI * S;",
      "  adjoint_next[0] = adj_S + adj_n_SI * p_SI;",
      "  adjoint_next[1] = adj_I + adj_n_IR * p_IR + adj_p_SI * shared.beta * dt * monty::math::exp(-shared.beta * I * dt / shared.N) / shared.N;",
      "  adjoint_next[2] = adj_R;",
      "  adjoint_next[3] = adj_cases_inc;",
      "  adjoint_next[4] = adj_p_SI * I * dt * monty::math::exp(-shared.beta * I * dt / shared.N) / shared.N + adj_beta;",
      "  adjoint_next[5] = adj_p_IR * dt * monty::math::exp(-shared.gamma * dt) + adj_gamma;",
      "  adjoint_next[6] = adj_I0;",
      "}"))

  expect_equal(
    generate_dust_system_adjoint_initial(dat),
    c(method_args$adjoint_initial,
      "  const auto adj_S = adjoint[0];",
      "  const auto adj_I = adjoint[1];",
      "  const auto adj_R = adjoint[2];",
      "  const auto adj_cases_inc = adjoint[3];",
      "  const auto adj_beta = adjoint[4];",
      "  const auto adj_gamma = adjoint[5];",
      "  const auto adj_I0 = adjoint[6];",
      "  adjoint_next[0] = adj_S;",
      "  adjoint_next[1] = adj_I;",
      "  adjoint_next[2] = adj_R;",
      "  adjoint_next[3] = adj_cases_inc;",
      "  adjoint_next[4] = adj_beta;",
      "  adjoint_next[5] = adj_gamma;",
      "  adjoint_next[6] = -adj_S + adj_I + adj_I0;",
      "}"))

  expect_equal(
    generate_dust_system_adjoint_compare_data(dat),
    c(method_args$adjoint_compare_data,
      "  const auto cases_inc = state[3];",
      "  const auto adj_S = adjoint[0];",
      "  const auto adj_I = adjoint[1];",
      "  const auto adj_R = adjoint[2];",
      "  const auto adj_cases_inc = adjoint[3];",
      "  const auto adj_beta = adjoint[4];",
      "  const auto adj_gamma = adjoint[5];",
      "  const auto adj_I0 = adjoint[6];",
      "  const real_type noise = 1 / shared.exp_noise;",
      "  const real_type lambda = cases_inc + noise;",
      "  const real_type adj_lambda = data.incidence / lambda - 1;",
      "  const real_type adj_noise = adj_lambda;",
      "  adjoint_next[0] = adj_S;",
      "  adjoint_next[1] = adj_I;",
      "  adjoint_next[2] = adj_R;",
      "  adjoint_next[3] = adj_lambda + adj_cases_inc;",
      "  adjoint_next[4] = adj_beta;",
      "  adjoint_next[5] = adj_gamma;",
      "  adjoint_next[6] = adj_I0;",
      "}"))
})
