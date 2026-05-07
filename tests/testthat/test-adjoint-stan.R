## Test symbolic adjoint gradients against Stan Math reverse-mode AD.
##
## For each model pattern (scalar, 1D array, mixing matrix),
## we:
##  1. Compile an odin model with adjoint and `~` comparison with data()
##     so that dust2 can compute the full likelihood gradient via adjoint.
##  2. Use the StanADTest package (pre-installed, uses Stan Math
##     reverse-mode AD via Rcpp) to compute single-step adjoints.
##  3. Verify that odin symbolic adjoint, Stan AD, and finite differences
##     all agree.
##
## StanADTest is a test-only dependency; skip if not available.
## It must be built with Apple clang (not Homebrew) on macOS to avoid
## libc++ ABI mismatch with RcppParallel's TBB.


test_that("Stan AD matches FD for scalar SIR single-step adjoint", {
  skip_if_not_installed("StanADTest")

  S <- 800; I <- 150; R <- 50
  beta <- 0.5; gamma <- 0.1
  state <- c(S, I, R)
  params <- c(beta, gamma)

  update_fn <- function(s, p) {
    N <- s[1] + s[2] + s[3]
    n_SI <- p[1] * s[1] * s[2] / N
    n_IR <- p[2] * s[2]
    c(s[1] - n_SI, s[2] + n_SI - n_IR, s[3] + n_IR)
  }

  set.seed(42)
  for (trial in 1:10) {
    adj <- rnorm(3)
    stan_grad <- StanADTest::stan_sir_step_adjoint(state, params, adj)

    eps <- 1e-7
    fd <- numeric(5)
    for (k in 1:3) {
      sp <- state; sp[k] <- sp[k] + eps
      sm <- state; sm[k] <- sm[k] - eps
      fd[k] <- sum(adj * (update_fn(sp, params) -
                           update_fn(sm, params))) / (2 * eps)
    }
    for (k in 1:2) {
      pp <- params; pp[k] <- pp[k] + eps
      pm <- params; pm[k] <- pm[k] - eps
      fd[3 + k] <- sum(adj * (update_fn(state, pp) -
                               update_fn(state, pm))) / (2 * eps)
    }

    expect_equal(as.numeric(stan_grad), fd, tolerance = 1e-6,
                 label = sprintf("trial %d", trial))
  }
})


test_that("Stan AD matches FD for 3-group mixing SIR single-step", {
  skip_if_not_installed("StanADTest")

  m_mat <- matrix(c(2.5, 0.5, 0.1,
                     0.5, 2.5, 0.5,
                     0.1, 0.5, 2.5), 3, 3)
  state <- c(800, 700, 900, 120, 180, 50, 80, 120, 50)
  params <- c(0.5, 0.1)

  update_fn <- function(st, p) {
    S <- st[1:3]; I <- st[4:6]; R <- st[7:9]
    N_total <- S + I + R
    prop_I <- ifelse(N_total == 0, 0, I / N_total)
    s_ij <- matrix(0, 3, 3)
    for (i in 1:3) for (j in 1:3)
      s_ij[i, j] <- m_mat[i, j] * prop_I[j]
    lambda <- p[1] * rowSums(s_ij)
    p_SI <- 1 - exp(-lambda)
    p_IR <- 1 - exp(-p[2])
    n_SI <- S * p_SI
    n_IR <- I * p_IR
    c(S - n_SI, I + n_SI - n_IR, R + n_IR)
  }

  set.seed(123)
  for (trial in 1:5) {
    adj <- rnorm(9)
    stan_grad <- StanADTest::stan_mixing_step_adjoint(
      state, params, m_mat, adj)

    eps <- 1e-7
    fd <- numeric(11)
    for (k in 1:9) {
      sp <- state; sp[k] <- sp[k] + eps
      sm <- state; sm[k] <- sm[k] - eps
      fd[k] <- sum(adj * (update_fn(sp, params) -
                           update_fn(sm, params))) / (2 * eps)
    }
    for (k in 1:2) {
      pp <- params; pp[k] <- pp[k] + eps
      pm <- params; pm[k] <- pm[k] - eps
      fd[9 + k] <- sum(adj * (update_fn(state, pp) -
                               update_fn(state, pm))) / (2 * eps)
    }

    expect_equal(as.numeric(stan_grad), fd, tolerance = 1e-5,
                 label = sprintf("mixing trial %d", trial))
  }
})


test_that("Stan AD matches FD for 2D array (age x vax) SIR", {
  skip_if_not_installed("StanADTest")

  suscept <- c(1.0, 0.3)
  state <- c(490, 390, 290, 490, 390, 290,
             8, 6, 4, 2, 2, 2,
             2, 4, 6, 8, 8, 8)
  params <- c(0.5, 0.1)
  na <- 3; nv <- 2; ns <- na * nv

  update_fn <- function(st, p) {
    S <- st[1:ns]; I <- st[(ns + 1):(2 * ns)]
    R <- st[(2 * ns + 1):(3 * ns)]
    total_I <- numeric(na); total_N <- numeric(na)
    for (i in 1:na) for (j in 1:nv) {
      idx <- i + (j - 1) * na
      total_I[i] <- total_I[i] + I[idx]
      total_N[i] <- total_N[i] + S[idx] + I[idx] + R[idx]
    }
    foi <- p[1] * total_I / total_N
    n_SI <- numeric(ns); n_IR <- numeric(ns)
    for (i in 1:na) for (j in 1:nv) {
      idx <- i + (j - 1) * na
      n_SI[idx] <- S[idx] * foi[i] * suscept[j]
      n_IR[idx] <- p[2] * I[idx]
    }
    c(S - n_SI, I + n_SI - n_IR, R + n_IR)
  }

  set.seed(77)
  for (trial in 1:5) {
    adj <- rnorm(3 * ns)
    stan_grad <- StanADTest::stan_2d_step_adjoint(
      state, params, suscept, adj)

    eps <- 1e-7
    n_total <- 3 * ns + 2
    fd <- numeric(n_total)
    for (k in seq_along(state)) {
      sp <- state; sp[k] <- sp[k] + eps
      sm <- state; sm[k] <- sm[k] - eps
      fd[k] <- sum(adj * (update_fn(sp, params) -
                           update_fn(sm, params))) / (2 * eps)
    }
    for (k in 1:2) {
      pp <- params; pp[k] <- pp[k] + eps
      pm <- params; pm[k] <- pm[k] - eps
      fd[3 * ns + k] <- sum(adj * (update_fn(state, pp) -
                                    update_fn(state, pm))) / (2 * eps)
    }

    expect_equal(as.numeric(stan_grad), fd, tolerance = 1e-5,
                 label = sprintf("2D trial %d", trial))
  }
})


test_that("odin symbolic adjoint matches FD for scalar SIR via unfilter", {
  gen <- odin({
    update(S) <- S - n_SI
    update(I) <- I + n_SI - n_IR
    update(R) <- R + n_IR
    update(cases_inc) <- n_SI

    initial(S) <- N - I0
    initial(I) <- I0
    initial(R) <- 0
    initial(cases_inc) <- 0

    n_SI <- beta * S * I / (S + I + R)
    n_IR <- gamma * I

    incidence <- data()
    noise <- 1e-6
    lambda <- cases_inc + noise
    incidence ~ Poisson(lambda)

    N <- parameter(1000)
    I0 <- parameter(10)
    beta <- parameter(0.5, differentiate = TRUE)
    gamma <- parameter(0.1, differentiate = TRUE)
  }, quiet = TRUE)

  time_start <- 0
  data <- data.frame(time = c(4, 8, 12, 16), incidence = 1:4)

  obj <- dust2::dust_unfilter_create(gen, time_start, data)
  packer <- monty::monty_packer(
    c("beta", "gamma"),
    fixed = list(N = 1000, I0 = 10))

  ll_model <- dust2::dust_likelihood_monty(obj, packer)

  theta <- c(0.5, 0.1)
  grad_odin <- ll_model$gradient(theta)

  eps <- 1e-5
  grad_fd <- numeric(2)
  for (k in 1:2) {
    tp <- theta; tp[k] <- tp[k] + eps
    tm <- theta; tm[k] <- tm[k] - eps
    grad_fd[k] <- (ll_model$density(tp) - ll_model$density(tm)) / (2 * eps)
  }

  expect_equal(grad_odin, grad_fd, tolerance = 1e-4,
               label = "odin adjoint vs FD for full SIR likelihood")
})


test_that("odin symbolic adjoint matches FD for mixing SIR via unfilter", {
  gen <- odin({
    update(S[]) <- S[i] - n_SI[i]
    update(I[]) <- I[i] + n_SI[i] - n_IR[i]
    update(R[]) <- R[i] + n_IR[i]
    update(cases_total_inc) <- sum(n_SI)

    initial(S[]) <- N[i] - I0[i]
    initial(I[]) <- I0[i]
    initial(R[]) <- 0
    initial(cases_total_inc) <- 0

    N_total[] <- S[i] + I[i] + R[i]
    prop_I[] <- if (N_total[i] == 0) 0 else I[i] / N_total[i]
    s_ij[, ] <- m[i, j] * prop_I[j]
    lambda_i[] <- beta * sum(s_ij[i, ])
    p_SI[] <- 1 - exp(-lambda_i[i])
    p_IR <- 1 - exp(-gamma)
    n_SI[] <- S[i] * p_SI[i]
    n_IR[] <- I[i] * p_IR

    incidence <- data()
    noise <- 1e-6
    obs_lambda <- cases_total_inc + noise
    incidence ~ Poisson(obs_lambda)

    beta <- parameter(0.5, differentiate = TRUE)
    gamma <- parameter(0.1, differentiate = TRUE)
    m <- parameter()
    N <- parameter()
    I0 <- parameter()

    dim(S, I, R, N, I0, N_total, prop_I, lambda_i, p_SI, n_SI, n_IR) <- 3
    dim(m, s_ij) <- c(3, 3)
  }, quiet = TRUE, check_bounds = FALSE)

  m_mat <- matrix(c(2.5, 0.5, 0.1,
                     0.5, 2.5, 0.5,
                     0.1, 0.5, 2.5), 3, 3)

  time_start <- 0
  data <- data.frame(time = 1:10, incidence = c(2, 5, 10, 15, 20,
                                                  18, 12, 8, 5, 3))

  obj <- dust2::dust_unfilter_create(gen, time_start, data)
  packer <- monty::monty_packer(
    c("beta", "gamma"),
    fixed = list(m = m_mat,
                 N = c(1000, 1000, 1000),
                 I0 = c(5, 3, 2)))

  ll_model <- dust2::dust_likelihood_monty(obj, packer)

  theta <- c(0.5, 0.1)
  grad_odin <- ll_model$gradient(theta)

  eps <- 1e-5
  grad_fd <- numeric(2)
  for (k in 1:2) {
    tp <- theta; tp[k] <- tp[k] + eps
    tm <- theta; tm[k] <- tm[k] - eps
    grad_fd[k] <- (ll_model$density(tp) - ll_model$density(tm)) / (2 * eps)
  }

  expect_equal(grad_odin, grad_fd, tolerance = 1e-4,
               label = "mixing adjoint vs FD for full likelihood")
})


test_that("odin adjoint and Stan AD both match FD for SIR", {
  skip_if_not_installed("StanADTest")

  gen <- odin({
    update(S) <- S - n_SI
    update(I) <- I + n_SI - n_IR
    update(R) <- R + n_IR
    update(cases_inc) <- n_SI

    initial(S) <- N - I0
    initial(I) <- I0
    initial(R) <- 0
    initial(cases_inc) <- 0

    n_SI <- beta * S * I / (S + I + R)
    n_IR <- gamma * I

    incidence <- data()
    noise <- 1e-6
    lambda <- cases_inc + noise
    incidence ~ Poisson(lambda)

    N <- parameter(1000)
    I0 <- parameter(10)
    beta <- parameter(0.5, differentiate = TRUE)
    gamma <- parameter(0.1, differentiate = TRUE)
  }, quiet = TRUE)

  time_start <- 0
  data <- data.frame(time = c(4, 8, 12, 16), incidence = 1:4)

  obj <- dust2::dust_unfilter_create(gen, time_start, data)
  packer <- monty::monty_packer(
    c("beta", "gamma"),
    fixed = list(N = 1000, I0 = 10))
  ll_model <- dust2::dust_likelihood_monty(obj, packer)

  theta <- c(0.5, 0.1)
  grad_odin <- ll_model$gradient(theta)

  ## FD gradient
  eps <- 1e-5
  grad_fd <- numeric(2)
  for (k in 1:2) {
    tp <- theta; tp[k] <- tp[k] + eps
    tm <- theta; tm[k] <- tm[k] - eps
    grad_fd[k] <- (ll_model$density(tp) - ll_model$density(tm)) / (2 * eps)
  }

  ## Odin adjoint matches FD
  expect_equal(grad_odin, grad_fd, tolerance = 1e-4,
               label = "odin vs FD")

  ## Stan single-step adjoint also matches FD for same equations
  state <- c(800, 150, 50)
  adj <- c(1, 0, 0)
  stan_g <- StanADTest::stan_sir_step_adjoint(state, c(0.5, 0.1), adj)

  update_fn <- function(s, p) {
    N <- s[1] + s[2] + s[3]
    n_SI <- p[1] * s[1] * s[2] / N
    n_IR <- p[2] * s[2]
    c(s[1] - n_SI, s[2] + n_SI - n_IR, s[3] + n_IR)
  }
  fd_step <- numeric(5)
  eps2 <- 1e-7
  for (k in 1:3) {
    sp <- state; sp[k] <- sp[k] + eps2
    sm <- state; sm[k] <- sm[k] - eps2
    fd_step[k] <- sum(adj * (update_fn(sp, c(0.5, 0.1)) -
                              update_fn(sm, c(0.5, 0.1)))) / (2 * eps2)
  }
  for (k in 1:2) {
    pp <- c(0.5, 0.1); pp[k] <- pp[k] + eps2
    pm <- c(0.5, 0.1); pm[k] <- pm[k] - eps2
    fd_step[3 + k] <- sum(adj * (update_fn(state, pp) -
                                  update_fn(state, pm))) / (2 * eps2)
  }
  expect_equal(as.numeric(stan_g), fd_step, tolerance = 1e-6,
               label = "stan single-step adjoint vs FD")
})
