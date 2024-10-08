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
    deriv(x) <- b + c
    a <- parameter(constant = TRUE)
    b <- parameter()
    c <- parameter(constant = FALSE)
  })
  expect_equal(res$parameters,
               data.frame(name = c("a", "b", "c"),
                          type = "real_type",
                          differentiate = FALSE,
                          constant = c(TRUE, FALSE, FALSE)))
})


test_that("parameters default to constant in face of differentiability", {
  res <- odin_parse({
    initial(x) <- a
    deriv(x) <- b + c
    a <- parameter(constant = TRUE)
    b <- parameter()
    c <- parameter(differentiate = TRUE, constant = FALSE)
  })
  expect_equal(res$parameters,
               data.frame(name = c("a", "b", "c"),
                          type = "real_type",
                          differentiate = c(FALSE, FALSE, TRUE),
                          constant = c(TRUE, TRUE, FALSE)))
})


test_that("fail informatively if recursive depenency in equations", {
  err <- expect_error(
    odin_parse({
      initial(x) <- a
      deriv(x) <- a + c
      a <- c + 1
      b <- a * 2
      c <- b / 2
    }),
    "Cyclic dependency detected within equations 'a', 'b', and 'c'")
})


test_that("prevent dependencies among variables in initial conditions", {
  ## ...at least for now
  expect_error(
    odin_parse({
      initial(a) <- 1
      initial(b) <- a + 1
      deriv(a) <- 0
      deriv(b) <- 0
    }),
    "Dependencies within initial conditions not yet supported")
})


test_that("unpack indirect variables in compare", {
  dat <- odin_parse({
    update(x) <- 1
    initial(x) <- 1
    p <- exp(x)
    d <- data()
    d ~ Poisson(p)
  })

  expect_equal(dat$phases$compare$unpack, "x")
  expect_equal(dat$phases$compare$equations, "p")
  expect_length(dat$phases$compare$compare, 1)
})


test_that("unpack self-referential variables", {
  dat <- odin_parse({
    update(a) <- a + 1
    initial(a) <- 1
  })
  expect_equal(dat$phases$update$unpack, "a")
})


test_that("Error if equations unused", {
  ## Also a regression test for partial prune (mrc-5798)
  expect_error(
    odin_parse({
      initial(a) <- 0
      update(a) <- 1
      b <- x[1]
      x[1] <- 1
      dim(x) <- 1
    }),
    "Unused equations: 'x' and 'b'")
})
