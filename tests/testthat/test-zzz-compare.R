test_that("can use vector data", {
  gen <- odin({
    initial(y[]) <- 0
    update(y[]) <- Normal(y[i], sd)
    dim(y, observed) <- len
    len <- parameter()
    sd <- parameter()
    observed <- data()
    observed[] ~ Normal(y[i], sd)
  }, debug = TRUE, quiet = TRUE)

  n_obs <- 5
  len <- 3
  d <- data.frame(
    time = seq_len(n_obs) * 4,
    observed = I(lapply(seq_len(n_obs), function(i) rnorm(len))))

  sys <- dust2::dust_system_create(gen, list(len = len, sd = 1))
  dust2::dust_system_run_to_time(sys, 4)
  y <- dust2::dust_system_state(sys)
  expect_equal(
    dust2::dust_system_compare_data(sys, list(observed = d$observed[[1]])),
    sum(dnorm(y, d$observed[[1]], 1, log = TRUE)))

  ## Cope with missing data as expected
  expect_equal(
    dust2::dust_system_compare_data(sys, list(observed = c(1, NA, 2))),
    sum(dnorm(y[-2], c(1, 2), 1, log = TRUE)))
  expect_equal(
    dust2::dust_system_compare_data(sys, list(observed = rep(NA_real_, 3))),
    0)
})
