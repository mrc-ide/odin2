test_that("can parse trivial system", {
  res <- odin_parse({
    initial(x) <- 0
    update(x) <- 0
  })
  expect_null(res)
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
