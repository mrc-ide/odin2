test_that("require that interpolation mode is one of the known types", {
  expect_error(
    parse_expr(quote(x <- interpolate(a, b, "wiggly")), NULL, NULL),
    "Invalid 'mode' argument to 'interpolate()'",
    fixed = TRUE)
  expect_error(
    parse_expr(quote(x <- interpolate(a, b, mode = TRUE)), NULL, NULL),
    "Invalid 'mode' argument to 'interpolate()'",
    fixed = TRUE)
  expect_error(
    parse_expr(quote(x <- interpolate(a, b)), NULL, NULL),
    "Invalid 'mode' argument to 'interpolate()'",
    fixed = TRUE)
})


test_that("arguments to interpolate must be symbols", {
  expect_error(
    parse_expr(quote(x <- interpolate(a[1, ], b, "constant")), NULL, NULL),
    "Expected 'time' argument to 'interpolate()' to be a symbol",
    fixed = TRUE)
  expect_error(
    parse_expr(quote(x <- interpolate(a, 1, "constant")), NULL, NULL),
    "Expected 'value' argument to 'interpolate()' to be a symbol",
    fixed = TRUE)
})


test_that("validate call to interpolate", {
  expect_error(
    parse_expr(quote(x <- interpolate(a, b, "constant", TRUE)), NULL, NULL),
    "Invalid call to 'interpolate()'",
    fixed = TRUE)
})


test_that("parse interpolation call", {
  res <- parse_expr(quote(x <- interpolate(a, b, "constant")), NULL, NULL)
  expect_null(res$special)
  expect_equal(res$lhs,
               list(name = "interpolate_x", array = NULL, name_data = "x"))
  expect_equal(res$rhs$type, "interpolate")
  expect_equal(
    res$rhs$expr,
    quote(OdinInterpolateAlloc(mode = "constant", time = "a", value = "b")))
  expect_equal(
    res$rhs$depends,
    list(functions = character(),
         variables = c("a", "b")))
})
