test_that("can compile a simple ode model", {
  skip_on_covr()
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
  expect_mapequal(attr(res, "properties"),
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

  expect_equal(coef(res),
               data_frame(name = c("N", "beta", "gamma", "I0"),
                          type = "real_type",
                          constant = c(TRUE, FALSE, FALSE, FALSE),
                          required = FALSE,
                          rank = 0))
})


test_that("can compile a discrete-time model that compares to data", {
  skip_on_covr()
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
  expect_mapequal(attr(res, "properties"),
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

  d$incidence[] <- NA_real_
  expect_equal(dust2::dust_system_compare_data(sys, d), 0)
})


test_that("can generate simple model with array", {
  skip_on_covr()
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

  r <- monty::monty_rng_create(n_streams = 10, seed = 42)
  expect_equal(state, rbind(colSums(monty::monty_random_n_normal(3, 0, 1, r))))
})


test_that("can generate model with interpolation", {
  skip_on_covr()
  gen <- odin({
    initial(x) <- 0
    update(x) <- a
    initial(y[]) <- 0
    update(y[]) <- b[i]
    nb <- 3
    nta <- 10
    ntb <- 20

    ta <- parameter()
    tb <- parameter()
    ya <- parameter()
    yb <- parameter()

    a <- interpolate(ta, ya, "linear")
    b <- interpolate(tb, yb, "linear")

    dim(ta) <- nta
    dim(ya) <- nta
    dim(tb) <- ntb
    dim(yb) <- c(nb, ntb)
    dim(b) <- nb
    dim(y) <- nb
  }, debug = TRUE, quiet = TRUE)

  ta <- seq(0, length.out = 10, by = 2)
  tb <- seq(0, length.out = 20)
  ya <- runif(length(ta))
  yb <- matrix(runif(3 * length(tb)), 3)
  pars <- list(ta = ta, tb = tb, ya = ya, yb = yb)

  sys <- dust2::dust_system_create(gen(), pars, 1, dt = 0.25)
  t_out <- seq(0, 18, by = 0.25)
  y <- dust2::dust_system_simulate(sys, t_out)

  ## There is some drama here with being off-by-one due to when we
  ## read and write interpolation values; this is expected but
  ## confusing.
  n <- length(t_out)
  expect_equal(y[1, ], c(0, approx(ta, ya, t_out)$y[-n]))
  expect_equal(y[2, ], c(0, approx(tb, yb[1, ], t_out)$y[-n]))
  expect_equal(y[3, ], c(0, approx(tb, yb[2, ], t_out)$y[-n]))
  expect_equal(y[4, ], c(0, approx(tb, yb[3, ], t_out)$y[-n]))
})


test_that("Can generate an ode system with output", {
  skip_on_covr()
  gen <- odin({
    initial(x[]) <- 1
    deriv(x[]) <- r[i] * x[i] * (1 - x[i] / K[i])
    output(tot) <- sum(x)
    r <- parameter()
    K <- parameter()
    n <- 5
    dim(x) <- n
    dim(r) <- n
    dim(K) <- n
  }, debug = TRUE, quiet = TRUE)

  logistic <- function(r, k, times, y0) {
    len <- max(length(r), length(k), length(y0))
    y <- vapply(times, function(t) k / (1 + (k / y0 - 1) * exp(-r * t)),
                numeric(len))
    rbind(y, colSums(y))
  }

  set.seed(1)
  pars <- list(r = runif(5, 0, 3), K = runif(5, 100, 1000))
  sys <- dust2::dust_system_create(gen, pars)
  dust2::dust_system_set_state_initial(sys)

  t <- 0:20
  y <- dust2::dust_system_simulate(sys, t)

  expect_equal(y, logistic(pars$r, pars$K, t, rep(1, 5)), tolerance = 1e-5)
})


test_that("can compile model with delays", {
  skip_if_not_installed("deSolve")
  gen <- odin({
    ylag <- delay(y, tau)
    initial(y) <- 0.5
    deriv(y) <- 0.2 * ylag * 1 / (1 + ylag^10) - 0.1 * y
    tau <- parameter(10, constant = TRUE)
  }, quiet = TRUE, debug = TRUE)

  rhs <- function(t, y, pars) {
    tau <- pars$tau
    if (t < tau) {
      ylag <- 0.5
    } else {
      ylag <- deSolve::lagvalue(t - tau)
    }
    list(0.2 * ylag * 1 / (1 + ylag^10) - 0.1 * y)
  }

  t <- seq(0, 300, length.out = 301)
  sys1 <- dust2::dust_system_create(gen)
  dust2::dust_system_set_state_initial(sys1)
  y1 <- dust2::dust_system_simulate(sys1, t)

  ## Compare against deSolve
  z1 <- deSolve::dede(0.5, t, rhs, list(tau = 10))
  expect_equal(drop(y1), z1[, 2], tolerance = 1e-5)

  sys2 <- dust2::dust_system_create(gen, list(tau = 20))
  dust2::dust_system_set_state_initial(sys2)
  y2 <- dust2::dust_system_simulate(sys2, t)
  z2 <- deSolve::dede(0.5, t, rhs, list(tau = 20))
  expect_equal(drop(y2), z2[, 2], tolerance = 1e-5)
})


test_that("can use output arrays", {
  gen <- odin({
    deriv(x[]) <- a[i]
    initial(x[]) <- i
    a[] <- sqrt(time / (x[i] + 1))
    output(a) <- TRUE
    dim(a, x) <- 5
  }, quiet = TRUE, debug = TRUE)
  sys <- dust2::dust_system_create(gen, list())
  dust2::dust_system_set_state_initial(sys)
  time <- seq(0, 10, length.out = 11)
  y <- dust2::dust_system_simulate(sys, time)
  yy <- dust2::dust_unpack_state(sys, y)
  expect_equal(yy$a, sqrt(rep(time, each = 5) / (yy$x + 1)))
})
