test_that("delay must be constant", {
  expect_error(
    odin_parse({
      ylag <- delay(y, tau)
      initial(y) <- 0.5
      deriv(y) <- 0.2 * ylag * 1 / (1 + ylag^10) - 0.1 * y
      tau <- parameter(10)
    }),
    "Delay time 'tau' is not constant")
  expect_error(
    odin_parse({
      ylag <- delay(y, y)
      initial(y) <- 0.5
      deriv(y) <- 0.2 * ylag * 1 / (1 + ylag^10) - 0.1 * y
      tau <- parameter(10)
    }),
    "Delay time 'y' is not constant")
})


test_that("can't delay expressions yet", {
  expect_error(
    odin_parse({
      y1 <- y + 1
      ylag <- delay(y1, 1)
      initial(y) <- 0.5
      deriv(y) <- 0.2 * ylag * 1 / (1 + ylag^10) - 0.1 * y
    }),
    "Delayed expressions are not yet supported")
})
