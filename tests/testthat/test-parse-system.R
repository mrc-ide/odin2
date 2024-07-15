test_that("At least one call to initial() is required", {
  expect_error(
    odin_parse({
      a <- 1
    }),
    "Did not find any call to 'initial()'",
    fixed = TRUE)

  expect_error(
    odin_parse({
      update(x) <- x + a
      a <- 1
    }),
    "Did not find any call to 'initial()'",
    fixed = TRUE)

  expect_error(
    odin_parse({
    }),
    "Did not find any call to 'initial()'",
    fixed = TRUE)
})


test_that("Don't support mixed ode/discrete time models yet", {
  expect_error(
    odin_parse({
      initial(x) <- 1
      deriv(x) <- 1
      initial(y) <- 1
      update(y) <- y + 1
    }),
    "Can't use both 'update()' and 'deriv()' within a single model yet",
    fixed = TRUE)
})


test_that("All variables with update() require initial()", {
  expect_error(
    odin_parse({
      initial(x) <- 1
      update(x) <- x + 1
      update(y) <- y + 1
    }),
    "Variables used in 'update()' do not have 'initial()' calls",
    fixed = TRUE)
})


test_that("All variables with initial() require a target", {
  expect_error(
    odin_parse({
      initial(x) <- 1
      initial(y) <- 1
    }),
    "Did not find any call to 'deriv()' or 'initial()'",
    fixed = TRUE)
  expect_error(
    odin_parse({
      initial(x) <- 1
      initial(y) <- 1
      deriv(x) <- 1
    }),
    "Variables defined with 'initial()' do not have 'deriv()' calls",
    fixed = TRUE)
  expect_error(
    odin_parse({
      initial(x) <- 1
      initial(y) <- 1
      update(x) <- 1
    }),
    "Variables defined with 'initial()' do not have 'update()' calls",
    fixed = TRUE)
})


test_that("Determine time type from parse", {
  res <- odin_parse({
    initial(x) <- 1
    initial(y) <- 1
    update(x) <- 1
    update(y) <- 1
  })
  expect_equal(res$time, "discrete")
  res <- odin_parse({
    initial(x) <- 1
    initial(y) <- 1
    deriv(x) <- 1
    deriv(y) <- 1
  })
  expect_equal(res$time, "continuous")
})


test_that("collect information about parameters from parse", {
  res <- odin_parse({
    initial(x) <- a
    deriv(x) <- b
    a <- parameter(constant = TRUE)
    b <- parameter()
    c <- parameter(constant = FALSE)
  })
  expect_equal(res$parameters,
               data.frame(name = c("a", "b", "c"),
                          differentiate = FALSE,
                          constant = c(TRUE, FALSE, FALSE)))
})


test_that("parameters default to constant in face of differentiability", {
  res <- odin_parse({
    initial(x) <- a
    deriv(x) <- b
    a <- parameter(constant = TRUE)
    b <- parameter()
    c <- parameter(differentiate = TRUE, constant = FALSE)
  })
  expect_equal(res$parameters,
               data.frame(name = c("a", "b", "c"),
                          differentiate = c(FALSE, FALSE, TRUE),
                          constant = c(TRUE, TRUE, FALSE)))
})
