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


test_that("can parse delayed array equation", {
  dat <- odin_parse({
    deriv(x) <- sum(y)
    initial(x) <- 1
    ## TODO: array size here, this implies that the size of y and z
    ## are the same, this can be done by the constraints mechanism or
    ## through the same way as interpolate does it.
    y <- delay(z, 2)
    dim(y) <- 5
    z[] <- x / i
    dim(z) <- 5
  })

  expect_setequal(dat$phases$build_shared$equations, c("dim_y", "dim_z"))
  expect_equal(dat$phases$update_shared$equations, character())
  expect_equal(dat$phases$deriv$equations, "y")
  expect_equal(nrow(dat$delays), 1)
  expect_equal(dat$delays$value[[1]],
               list(what = "z", variables = "x", equations = "z"))
})
