test_that("can compile a simple ode model", {
  res <- odin({
    deriv(S) <- -beta * S * I / N
    deriv(I) <- beta * S * I / N - gamma * I
    deriv(R) <- gamma * I
    N <- parameter(1000, constant = TRUE)
    initial(S) <- N - I0
    initial(I) <- I0
    initial(R) <- 0
    beta <- parameter(0.2)
    gamma <- parameter(0.1)
    I0 <- parameter(10)
  }, debug = TRUE, quiet = TRUE)

  expect_s3_class(res(), "dust_system_generator")
  expect_mapequal(res()$properties,
                  list(time_type = "continuous",
                       has_compare = FALSE,
                       has_adjoint = FALSE))

  pars <- list(N = 100, beta = 0.2, gamma = 0.1, I0 = 1)
  sys <- dust2::dust_system_create(res(), pars, 1)
  expect_s3_class(sys, "dust_system")
  dust2::dust_system_set_state_initial(sys)
  expect_equal(dust2::dust_system_state(sys), c(99, 1, 0))

  dust2::dust_system_run_to_time(sys, 10)
  s <- dust2::dust_system_state(sys)

  cmp <- local({
    sys <- dust2::dust_system_create(dust2:::sirode(), pars, 1)
    dust2::dust_system_set_state_initial(sys)
    dust2::dust_system_run_to_time(sys, 10)
    dust2::dust_system_state(sys)
  })

  expect_equal(s, cmp[1:3])
})


test_that("can compile a discrete-time model that compares to data", {
  res <- odin({
    ## Core equations for transitions between compartments:
    update(S) <- S - n_SI
    update(I) <- I + n_SI - n_IR
    update(R) <- R + n_IR
    update(cases_cumul) <- cases_cumul + n_SI
    update(cases_inc) <- n_SI # for now, needs the resetting implemented!

    ## Individual probabilities of transition:
    p_SI <- 1 - exp(-beta * I / N * dt) # S to I
    p_IR <- 1 - exp(-gamma * dt) # I to R

    ## Draws from binomial distributions for numbers changing between
    ## compartments:
    n_SI <- S * p_SI # rbinom(S, p_SI)
    n_IR <- I * p_IR # rbinom(I, p_IR)

    ## Initial states:
    initial(S) <- N - I0
    initial(I) <- I0
    initial(R) <- 0
    initial(cases_cumul) <- 0
    initial(cases_inc) <- 0

    ## User defined parameters - default in parentheses:
    N <- parameter(1000)
    I0 <- parameter(10)
    beta <- parameter(0.2)
    gamma <- parameter(0.1)
    exp_noise <- parameter(1e6)

    ## Data and comparison
    noise <- 1 / exp_noise # Exponential(rate = exp_noise)
    incidence <- data()
    lambda <- cases_inc + noise
    incidence ~ Poisson(lambda)
  }, debug = TRUE, quiet = TRUE)

  expect_s3_class(res(), "dust_system_generator")
  expect_mapequal(res()$properties,
                  list(time_type = "discrete",
                       has_compare = TRUE,
                       has_adjoint = FALSE))

  pars <- list(N = 1000, beta = 0.2, gamma = 0.1, I0 = 10,
               exp_noise = 1e6)
  d <- list(incidence = 5)

  sys <- dust2::dust_system_create(res(), pars, 1, deterministic = TRUE)
  expect_s3_class(sys, "dust_system")
  dust2::dust_system_set_state_initial(sys)
  expect_equal(dust2::dust_system_state(sys), c(990, 10, 0, 0, 0))

  dust2::dust_system_run_to_time(sys, 10)
  state <- dust2::dust_system_state(sys)
  density <- dust2::dust_system_compare_data(sys, d)

  cmp <- local({
    sys <- dust2::dust_system_create(dust2:::sir(), pars, 1,
                                     deterministic = TRUE)
    dust2::dust_system_set_state_initial(sys)
    dust2::dust_system_run_to_time(sys, 10)
    list(state = dust2::dust_system_state(sys),
         density = dust2::dust_system_compare_data(sys, d))
  })

  expect_equal(state, cmp$state)
  expect_equal(density, cmp$density)
})


test_that("can generate simple model with array", {
  gen <- odin({
    initial(x) <- 0
    update(x) <- a[1] + a[2] + a[3]
    n <- 2
    a[] <- Normal(0, 1)
    dim(a) <- n + 1
  }, debug = TRUE, quiet = TRUE)

  sys <- dust2::dust_system_create(gen(), list(), 10, seed = 42)
  dust2::dust_system_run_to_time(sys, 1)
  state <- dust2::dust_system_state(sys)

  r <- monty::monty_rng$new(10, seed = 42)
  expect_equal(state, rbind(colSums(r$normal(3, 0, 1))))
})
