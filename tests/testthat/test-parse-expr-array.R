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


test_that("require constant array size", {
  expect_error(
    parse_expr(quote(dim(a) <- n), NULL, NULL),
    "Non-constant sized dimensions are not yet supported")
})


test_that("can't use array access with higher rank than the lhs implies", {
  expect_error(
    parse_expr(quote(a[] <- x[j]), NULL, NULL),
    "Invalid index access used on rhs of equation: 'j'")
})


test_that("array equations require a corresponding dim()", {
  expect_error(
    odin_parse({
      initial(x) <- 1
      update(x) <- a[0]
      a[] <- 1
    }),
    "Missing 'dim()' for expression assigned as an array: 'a'",
    fixed = TRUE)
})


test_that("array equations assigning with brackets", {
  expect_error(
    odin_parse({
      initial(x) <- 1
      update(x) <- a[0]
      dim(a) <- 3
      a <- 1
    }),
    "Array expressions must always use '[]' on the lhs",
    fixed = TRUE)
})
