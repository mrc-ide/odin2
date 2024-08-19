test_that("can parse trivial system", {
  res <- odin_parse({
    initial(x) <- 0
    update(x) <- 0
  })
  expect_equal(res$time, "discrete")
  expect_equal(res$class, "odin")
  expect_equal(res$variables, "x")
  expect_equal(nrow(res$parameters), 0)
  expect_equal(nrow(res$data), 0)
})


test_that("can parse dust MVP system", {
  ## Silly system that will work towards being one of the MVPs for
  ## compilation to dust; contains basically everything we are
  ## interested in.
  res <- odin_parse({
    z <- x + a
    update(x) <- x + z * 2 + p
    a <- 1 * b
    b <- parameter()
    p <- parameter(constant = TRUE)
    initial(x) <- 0
    d <- data()
    compare(d) ~ Normal(x, 1)
  })

  expect_equal(res$time, "discrete")
  expect_equal(res$class, "odin")
  expect_equal(res$variables, "x")
  expect_equal(res$parameters,
               data_frame(name = c("b", "p"),
                          differentiate = FALSE,
                          constant = c(FALSE, TRUE)))
  expect_equal(res$data, data_frame(name = "d"))
})


test_that("throw error with context", {
  path <- withr::local_tempfile()
  writeLines(c("initial(x) <- a",
               "update(x) <- x + b",
               "b<-parameter(invalid=TRUE)",
               "a <- 5"),
             path)
  err <- expect_error(
    odin_parse(path),
    "Invalid call to 'parameter()'",
    fixed = TRUE,
    class = "odin_parse_error")
  expect_equal(
    err$src,
    list(list(value = quote(b <- parameter(invalid = TRUE)),
              index = 3,
              start = 3,
              end = 3,
              str = c("b<-parameter(invalid=TRUE)"))))
  expect_match(
    conditionMessage(err),
    "Context:\n3| b<-parameter(invalid=TRUE)",
    fixed = TRUE)
})


test_that("throw error with context where source code unavailable", {
  err <- expect_error(
    odin_parse({
      initial(x) <- a
      update(x) <- x + b
      b <- parameter(invalid = TRUE)
      a <- 5
    }),
    "Invalid call to 'parameter()'",
    fixed = TRUE,
    class = "odin_parse_error")
  expect_match(
    conditionMessage(err),
    "Context:\nb <- parameter(invalid = TRUE)",
    fixed = TRUE)
})


test_that("throw stochastic parse error sensibly", {
  err <- expect_error(
    odin_parse({
      initial(x) <- 0
      update(x) <- Normal(mu = 0, sd = 1)
    }),
    "Invalid call to 'Normal()'",
    fixed = TRUE)
  expect_match(
    conditionMessage(err),
    deparse(quote(update(x) <- Normal(mu = 0, sd = 1))),
    fixed = TRUE)
})


test_that("can parse system that resets", {
  d <- odin_parse({
    update(x) <- x + 1
    initial(x, zero_every = 4) <- 0
    update(y) <- y + 1
    initial(y) <- 0
  })
  expect_equal(d$zero_every, list(x = 4))
})


test_that("zero_reset requires that initial conditions are zero", {
  expect_error(
    odin_parse({
      update(x) <- x + 1
      initial(x, zero_every = 1) <- 1
    }),
    "Initial condition of periodically zeroed variable must be 0")
})


test_that("zero_reset requires an integer argument", {
  expect_error(
    odin_parse({
      update(x) <- x + 1
      initial(x, zero_every = 1.4) <- 1
    }),
    "Argument to 'zero_every' must be an integer")
  expect_error(
    odin_parse({
      update(x) <- x + 1
      initial(x, zero_every = a) <- 1
    }),
    "Argument to 'zero_every' must be an integer")
})


test_that("can parse ode system that resets", {
  d <- odin_parse({
    deriv(x) <- 1
    initial(x, zero_every = 4) <- 0
    deriv(y) <- 1
    initial(y) <- 0
  })
  expect_equal(d$zero_every, list(x = 4))
})


test_that("parse systems that require shared storage", {
  dat <- odin_parse({
    a <- 1
    b <- parameter()
    c <- a + b
    initial(x) <- 1
    update(x) <- x + c
  })

  expect_equal(dat$storage$contents$shared,
               c("a", "b", "c"))
})


test_that("can parse systems that involve arrays in internal", {
  d <- odin_parse({
    initial(x) <- 1
    update(x) <- a[1] + a[2]
    a[] <- Normal(0, 1)
    dim(a) <- 2
  })

  expect_equal(d$storage$location[["a"]], "internal")
  expect_equal(
    d$storage$arrays,
    data_frame(name = "a", rank = 1, dims = I(list(2)), size = I(list(2))))
  expect_equal(
    d$equations$a$lhs$array,
    list(list(name = "i", is_range = TRUE, from = 1, to = 2)))
})


test_that("can parse systems that involve arrays in shared", {
  d <- odin_parse({
    initial(x) <- 1
    update(x) <- a[1] + a[2] + a[3]
    a[] <- i
    dim(a) <- 3
  })

  expect_equal(d$storage$location[["a"]], "shared")
  expect_equal(
    d$storage$arrays,
    data_frame(name = "a", rank = 1, dims = I(list(3)), size = I(list(3))))
  expect_equal(
    d$equations$a$lhs$array,
    list(list(name = "i", is_range = TRUE, from = 1, to = 3)))
})


test_that("pack system entirely composed of arrays", {
  d <- odin_parse({
    initial(x[]) <- 1
    update(x[]) <- x[i] * 2
    initial(y[]) <- 1
    update(y[]) <- y[i] / x[i]
    dim(x) <- 2
    dim(y) <- 2
  })
  expect_equal(
    d$storage$packing$state,
    data_frame(name = c("x", "y"),
               rank = 1,
               dims = I(list(2, 2)),
               size = I(list(2, 2)),
               offset = I(list(0, 2))))
})


test_that("pack system of mixed arrays and scalars", {
  d <- odin_parse({
    initial(x[]) <- 1
    update(x[]) <- x[i] * 2
    initial(y) <- 1
    update(y) <- y / (x[1] + x[2])
    dim(x) <- 2
  })
  expect_equal(
    d$storage$packing$state,
    data_frame(name = c("x", "y"),
               rank = c(1, 0),
               dims = I(list(2, NULL)),
               size = I(list(2, 1)),
               offset = I(list(0, 2))))
})
