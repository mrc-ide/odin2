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
