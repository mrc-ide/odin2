test_that("can parse trivial print statement", {
  expect_equal(
    parse_expr(quote(print("{a}")), NULL, NULL),
    list(special = "print",
         rhs = list(type = "print"),
         string = "{a}",
         inputs = list(list(expr = quote(a), format = NULL)),
         depends = list(functions = character(), variables = "a"),
         when = NULL))
})


test_that("debug string requires at least one variable", {
  expect_error(
    parse_expr(quote(print("a string")), NULL, NULL),
    "Invalid 'print()' expression does not reference any values",
    fixed = TRUE)
})


test_that("require that the first argument to print() is a string", {
  expect_error(
    parse_expr(quote(print(TRUE)), NULL, NULL),
    "Expected the first argument to 'print()' to be a string",
    fixed = TRUE)
})


test_that("cope with malformed glue string", {
  expect_error(
    parse_expr(quote(print("a {string")), NULL, NULL),
    "Failed to parse 'a {string' with 'glue'", fixed = TRUE)
})



test_that("debug string requires at least one variable", {
  expect_error(
    parse_expr(quote(print("a string")), NULL, NULL),
    "Invalid 'print()' expression does not reference any values",
    fixed = TRUE)
})


test_that("error if glue string component is malformed", {
  expect_error(
    parse_expr(quote(print("a {**}")), NULL, NULL),
    "Failed to parse debug string 'a {**}': '**' is not valid",
    fixed = TRUE)
  skip_on_cran() # probably platform dependent
  expect_error(
    parse_expr(quote(print("a {x; qqq}")), NULL, NULL),
    "Failed to parse debug string 'a {x; qqq}': 'x; qqq' is not valid",
    fixed = TRUE)
})
