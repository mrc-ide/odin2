test_that("can compute gradient", {
  sir <- odin({
    p_IR <- 1 - exp(-gamma * dt)
    S0 <- 1000
    N <- S + I + R
    p_inf <- beta * I / N * dt
    p_SI <- 1 - exp(-p_inf)
    n_SI <- Binomial(S, p_SI)
    n_IR <- Binomial(I, p_IR)
    update(S) <- S - n_SI
    update(I) <- I + n_SI - n_IR
    update(R) <- R + n_IR
    update(cases_inc) <- if (time %% 1 == 0) n_SI else cases_inc + n_SI
    update(cases_cumul) <- cases_cumul + n_SI
    initial(S) <- S0
    initial(I) <- I0
    initial(R) <- 0
    initial(cases_inc) <- 0
    initial(cases_cumul) <- 0
    beta <- parameter(0.2, differentiate = TRUE)
    gamma <- parameter(0.1, differentiate = TRUE)
    I0 <- parameter(10, differentiate = TRUE)
    cases_observed <- data()
    compare(cases_observed) ~ Poisson(cases_inc)
  }, quiet = TRUE, debug = TRUE)

  expect_mapequal(sir()$properties,
                  list(time_type = "discrete",
                       has_compare = TRUE,
                       has_adjoint = TRUE))

  time_start <- 0
  time <- c(4, 8, 12, 16)
  data <- lapply(1:4, function(i) list(cases_observed = i))

  obj <- dust2::dust_unfilter_create(sir(), time_start, time, data)
  x <- c(beta = 0.1, gamma = 0.2, I0 = 10)
  ll <- dust2::dust_unfilter_run(obj, as.list(x), adjoint = TRUE)
  gr <- dust2::dust_unfilter_last_gradient(obj)

  expect_identical(ll, dust2::dust_unfilter_run(obj, as.list(x)))

  gr_num <- numDeriv::grad(
    function(x) dust2::dust_unfilter_run(obj, as.list(x)), x,
    method = "Richardson", method.args = list(r = 6))

  expect_equal(gr_num, gr, tolerance = 1e-4)
})
