test_that("Can parse compare expression", {
  res <- parse_expr(quote(compare(x) ~ Normal(0, 1)), NULL, NULL)
  expect_equal(res$special, "compare")
  expect_equal(res$rhs$type, "compare")
  expect_equal(res$rhs$density$cpp, "normal")
  expect_equal(res$rhs$args, list(quote(x), 0, 1))
  expect_equal(res$rhs$depends,
               list(functions = "Normal", variables = "x"))
})


test_that("compare expressions must use '~'", {
  expect_error(
    parse_expr(quote(compare(x) <- Normal(0, 1)), NULL, NULL),
    "'compare()' expressions must use '~', not '<-'",
    fixed = TRUE)
})


test_that("only compare expressions may use '~'", {
  expect_error(
    parse_expr(quote(initial(x) ~ 1), NULL, NULL),
    "Expected the lhs of '~' to be a 'compare()' call",
    fixed = TRUE)
  expect_error(
    parse_expr(quote(x ~ 1), NULL, NULL),
    "Expected the lhs of '~' to be a 'compare()' call",
    fixed = TRUE)
})


test_that("compare() calls must wrap symbols", {
  expect_error(
    parse_expr(quote(compare(1) ~ Normal(0, 1)), NULL, NULL),
    "Expected the argument of 'compare()' to be a symbol",
    fixed = TRUE)
  expect_error(
    parse_expr(quote(compare(f(x)) ~ Normal(0, 1)), NULL, NULL),
    "Expected the argument of 'compare()' to be a symbol",
    fixed = TRUE)
  expect_error(
    parse_expr(quote(compare(x[]) ~ Normal(0, 1)), NULL, NULL),
    "Expected the argument of 'compare()' to be a symbol",
    fixed = TRUE)
})


test_that("parse compare call rhs as distributions", {
  expect_error(
    parse_expr(quote(compare(x) ~ 1), NULL, NULL),
    "The rhs of '~' is not a function call")
  expect_error(
    parse_expr(quote(compare(x) ~ Foo(0, 1)), NULL, NULL),
    "Unknown distribution 'Foo'")
  expect_error(
    parse_expr(quote(compare(x) ~ Normal(mu = 1)), NULL, NULL),
    "Invalid call to 'Normal()'",
    fixed = TRUE)
})
