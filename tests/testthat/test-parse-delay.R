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


test_that("can parse simple delay", {
  dat <- odin_parse({
    ylag <- delay(y, 1)
    initial(y) <- 0.5
    deriv(y) <- 0.2 * ylag * 1 / (1 + ylag^10) - 0.1 * y
  })
  expect_equal(
    dat$delays,
    data.frame(name = "ylag",
               type = "variable",
               in_rhs = TRUE,
               in_output = FALSE,
               by = I(list(1)),
               value = I(list(list(variables = "y", equations = NULL)))))
})
