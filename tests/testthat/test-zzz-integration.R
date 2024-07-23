test_that("can compile a simple ode model", {
  dat <- odin_parse({
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
  })

  code <- generate_dust_system(dat)
  path <- "dust.cpp"
  unlink(path)
  writeLines(code, path)
  res <- dust2::dust_compile(path, debug = TRUE, quiet = TRUE)
  expect_s3_class(res(), "dust_system_generator")
  expect_mapequal(res()$properties,
                  list(time_type = "continuous",
                       has_compare = FALSE))

  pars <- list(N = 100, beta = 0.2, gamma = 0.1, I0 = 1)
  sys <- dust2::dust_system_create(res(), pars, 1)
  expect_s3_class(sys, "dust_system")
  dust2::dust_system_set_state_initial(sys)
  expect_equal(dust2::dust_system_state(sys), cbind(c(99, 1, 0)))

  dust2::dust_system_run_to_time(sys, 10)
  s <- dust2::dust_system_state(sys)

  cmp <- local({
    sys <- dust2::dust_system_create(dust2:::sirode(), pars, 1)
    dust2::dust_system_set_state_initial(sys)
    dust2::dust_system_run_to_time(sys, 10)
    dust2::dust_system_state(sys)
  })

  expect_equal(s, cmp[1:3, , drop = FALSE])
})
