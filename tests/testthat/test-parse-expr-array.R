test_that("can validate array index", {
  expect_equal(
    parse_expr_check_lhs_index(rlang::missing_arg(), 1, NULL, NULL),
               list(name = "i", is_range = TRUE, from = 1, to = Inf))
  expect_equal(
    parse_expr_check_lhs_index(1, 1, NULL, NULL),
    list(name = "i", is_range = FALSE, at = 1))
  expect_equal(
    parse_expr_check_lhs_index(quote(a), 1, NULL, NULL),
    list(name = "i", is_range = FALSE, at = quote(a)))
  expect_error(
    parse_expr_check_lhs_index(FALSE, 1, NULL, NULL),
    "Invalid value for array index lhs")

  expect_equal(
    parse_expr_check_lhs_index(quote(1:5), 1, NULL, NULL),
    list(name = "i", is_range = TRUE, from = 1, to = 5))
  expect_equal(
    parse_expr_check_lhs_index(quote(a:b), 1, NULL, NULL),
    list(name = "i", is_range = TRUE, from = quote(a), to = quote(b)))

  expect_error(
    parse_expr_check_lhs_index(quote(sqrt(a:b)), 1, NULL, NULL),
    "Invalid use of range operator ':' on lhs of array assignment")
  expect_error(
    parse_expr_check_lhs_index(quote(a:(b:c)), 1, NULL, NULL),
    "Invalid use of range operator ':' on lhs of array assignment")
  expect_error(
    parse_expr_check_lhs_index(quote(f(1)), 1, NULL, NULL),
    "Invalid function used in lhs of array assignment: 'f'")
  expect_error(
    parse_expr_check_lhs_index(quote(-a), 1, NULL, NULL),
    "Invalid use of unary minus in lhs of array assignment")
  expect_error(
    parse_expr_check_lhs_index(quote(i), 1, NULL, NULL),
    "Invalid use of special variable in lhs of array assignment: 'i'")
  expect_error(
    parse_expr_check_lhs_index(quote(i + 1), 1, NULL, NULL),
    "Invalid use of special variable in lhs of array assignment: 'i'")
})


test_that("can parse array expression", {
  res <- parse_expr(quote(a[] <- 1), NULL, NULL)
  expect_null(res$special)
  expect_equal(res$lhs$name, "a")
  expect_equal(res$lhs$array,
               list(list(name = "i",
                         is_range = TRUE,
                         from = 1,
                         to = Inf)))
  expect_equal(res$rhs$type, "expression")
  expect_equal(res$rhs$expr, 1)
})
