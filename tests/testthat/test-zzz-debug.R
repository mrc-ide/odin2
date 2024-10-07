test_that("can compile model with debug enabled", {
  expect_no_error(
    odin({
      p_IR <- 1 - exp(-gamma * dt)
      N <- parameter(1000)
      p_SI <- 1 - exp(-(beta * I / N * dt))
      n_SI <- Binomial(S, p_SI)
      n_IR <- Binomial(I, p_IR)
      update(S) <- S - n_SI
      update(I) <- I + n_SI - n_IR
      update(R) <- R + n_IR
      initial(S) <- N - I0
      initial(I) <- I0
      initial(R) <- 0
      beta <- parameter(0.2)
      gamma <- parameter(0.1)
      I0 <- parameter(10)

      debug(phase = "update", when = I < 10 && time > 20)
    }, debug = TRUE, quiet = TRUE)
  )
})
