test_that("Error if undefined variables are used", {
  expect_error(
    odin_parse({
      initial(x) <- a
      update(x) <- b
      b <- 1 + y
    }),
    "Unknown variables used in odin code: 'a' and 'y'",
    class = "odin_parse_error")
})


test_that("Error if undefined variables used in dead code", {
  ## We might relax this one later, but it's probably a good thing
  expect_error(
    odin_parse({
      initial(x) <- 1
      update(x) <- 1
      a <- b + 1
    }),
    "Unknown variable used in odin code: 'b'",
    class = "odin_parse_error")
})


test_that("magic variable 'dt' is available in discrete time models", {
  expect_no_error(
    odin_parse({
      initial(x) <- 1
      update(x) <- r * dt
      r <- parameter(1)
    }))
})


test_that("Can't use 'dt' in continuous time models", {
  expect_error(
    odin_parse({
      initial(a) <- 1
      deriv(a) <- r * dt
      r <- parameter(1)
    }),
    "Cannot use 'dt' in a continuous time (ODE) model",
    class = "odin_parse_error",
    fixed = TRUE)
})
