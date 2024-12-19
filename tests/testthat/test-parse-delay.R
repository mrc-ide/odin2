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
    data.frame(
      name = "ylag",
      type = "variable",
      by = I(list(1)),
      value = I(list(list(what = "y", variables = "y", equations = NULL))),
      in_rhs = TRUE,
      in_output = FALSE))
})


test_that("can parse delayed array equation", {
  dat <- odin_parse({
    deriv(x) <- sum(y)
    initial(x) <- 1
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


test_that("delay expressions need to involve variables", {
  expect_error(
    odin_parse({
      deriv(x) <- a
      initial(x) <- 1
      b <- 10
      a <- delay(b, 1)
    }),
    "Invalid delay expression 'a' does not involve any variables")
})


test_that("delay expressions cannot involve data", {
  expect_error(
    odin_parse({
      deriv(x) <- 1
      deriv(y) <- a
      initial(x) <- 1
      initial(y) <- 1
      d <- data()
      b <- x + d
      a <- delay(b, 1)
    }),
    "Invalid delay expression 'a' depends on data (via 'b')", fixed = TRUE)
})


test_that("delay and target must match", {
  expect_error(
    odin_parse({
      initial(x) <- 0
      deriv(x) <- a
      b <- x + 1
      a <- delay(b, 1)
      dim(a) <- 1
    }),
    "Invalid dimensionality of 'a', expected a scalar following 'b'")
})


test_that("delay and target must have compatible sizes", {
  expect_error(
    odin_parse({
      initial(x) <- 0
      deriv(x) <- sum(a)
      a <- delay(b, 1)
      b[] <- x / i
      dim(b) <- 3
      dim(a) <- 2
    }),
    "Out of range write of 'a' in 'a <- delay(b, 1)'",
    fixed = TRUE)
  expect_error(
    odin_parse({
      initial(x) <- 0
      deriv(x) <- sum(a)
      a <- delay(b, 1)
      b[] <- x / i
      dim(b) <- 2
      dim(a) <- 3
    }),
    "Out of range read of 'b' in 'a <- delay(b, 1)'",
    fixed = TRUE)
})
