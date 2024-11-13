test_that("can parse delay expression", {
  res <- parse_expr(quote(a <- delay(b, c)), NULL, NULL)
  expect_equal(res$special, "delay")
  expect_equal(res$rhs$type, "delay")
  expect_equal(res$rhs$expr, quote(OdinDelay(what = "b", by = c)))
  expect_equal(res$rhs$depends,
               list(functions = character(), variables = character()))
})


test_that("delay must be assigned to a symbol", {
  expect_error(
    parse_expr(quote(update(a) <- delay(b, c)), NULL, NULL),
    "Calls to 'delay()' must be assigned to a symbol",
    fixed = TRUE)
  expect_error(
    parse_expr(quote(a <- delay(thing = b, c)), NULL, NULL),
    "Invalid call to 'delay()'",
    fixed = TRUE)
})


test_that("validate call to delay", {
  expect_error(
    parse_expr(quote(a <- delay(b, c, d)), NULL, NULL),
    "Invalid call to 'delay()'",
    fixed = TRUE)
  expect_error(
    parse_expr(quote(a <- delay(thing = b, c)), NULL, NULL),
    "Invalid call to 'delay()'",
    fixed = TRUE)
  expect_error(
    parse_expr(quote(a <- delay()), NULL, NULL),
    "Invalid call to 'delay()'",
    fixed = TRUE)
  expect_error(
    parse_expr(quote(a <- delay(b)), NULL, NULL),
    "Invalid call to 'delay()'",
    fixed = TRUE)
})


test_that("'what' must be a symbol", {
  expect_error(
    parse_expr(quote(a <- delay(2, c)), NULL, NULL),
    "Expected 'what' argument to 'delay()' to be a symbol",
    fixed = TRUE)
  expect_error(
    parse_expr(quote(a <- delay(c, what = 2)), NULL, NULL),
    "Expected 'what' argument to 'delay()' to be a symbol",
    fixed = TRUE)
})


test_that("'by' must be a symbol or number", {
  expect_error(
    parse_expr(quote(a <- delay(c, TRUE)), NULL, NULL),
    "Expected 'by' argument to 'delay()' to be a number or symbol",
    fixed = TRUE)
})
