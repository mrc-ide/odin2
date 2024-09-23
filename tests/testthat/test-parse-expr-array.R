test_that("can validate array index", {
  expect_equal(
    parse_expr_check_lhs_index(
      "x", 1, rlang::missing_arg(), NULL, NULL),
    list(name = "i", type = "range", from = 1, to = quote(OdinDim("x", 1))))
  expect_equal(
    parse_expr_check_lhs_index("x", 1, 1, NULL, NULL),
    list(name = "i", type = "single", at = 1))
  expect_equal(
    parse_expr_check_lhs_index("x", 1, quote(a), NULL, NULL),
    list(name = "i", type = "single", at = quote(a)))
  expect_error(
    parse_expr_check_lhs_index("x", 1, FALSE, NULL, NULL),
    "Invalid value for array index lhs")

  expect_equal(
    parse_expr_check_lhs_index("x", 1, quote(1:5), NULL, NULL),
    list(name = "i", type = "range", from = 1, to = 5))
  expect_equal(
    parse_expr_check_lhs_index("x", 1, quote(a:b), NULL, NULL),
    list(name = "i", type = "range", from = quote(a), to = quote(b)))

  expect_error(
    parse_expr_check_lhs_index("x", 1, quote(sqrt(a:b)), NULL, NULL),
    "Invalid use of range operator ':' on lhs of array assignment")
  expect_error(
    parse_expr_check_lhs_index("x", 1, quote(a:(b:c)), NULL, NULL),
    "Invalid use of range operator ':' on lhs of array assignment")
  expect_error(
    parse_expr_check_lhs_index("x", 1, quote(f(1)), NULL, NULL),
    "Invalid function used in lhs of array assignment: 'f'")
  expect_error(
    parse_expr_check_lhs_index("x", 1, quote(-a), NULL, NULL),
    "Invalid use of unary minus in lhs of array assignment")
  expect_error(
    parse_expr_check_lhs_index("x", 1, quote(i), NULL, NULL),
    "Invalid use of special variable in lhs of array assignment: 'i'")
  expect_error(
    parse_expr_check_lhs_index("x", 1, quote(i + 1), NULL, NULL),
    "Invalid use of special variable in lhs of array assignment: 'i'")
})


test_that("can parse array expression", {
  res <- parse_expr(quote(a[] <- 1), NULL, NULL)
  expect_null(res$special)
  expect_equal(res$lhs$name, "a")
  expect_equal(res$lhs$array,
               list(list(name = "i",
                         type = "range",
                         from = 1,
                         to = quote(OdinDim("a", 1L)))))
  expect_equal(res$rhs$type, "expression")
  expect_equal(res$rhs$expr, 1)
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


test_that("can parse call to a whole array", {
  res <- parse_expr(quote(a <- sum(x)), NULL, NULL)
  expect_equal(res$rhs$expr,
               quote(OdinReduce("sum", "x", index = NULL)))
  expect_equal(res$rhs$depends,
               list(functions = "sum", variables = "x"))
})


test_that("all empty index on sum is a complete sum", {
  expr <- quote(OdinReduce("sum", "x", index = NULL))
  expect_equal(parse_expr(quote(a <- sum(x[])), NULL, NULL)$rhs$expr, expr)
  expect_equal(parse_expr(quote(a <- sum(x[, ])), NULL, NULL)$rhs$expr, expr)
  expect_equal(parse_expr(quote(a <- sum(x[, , ])), NULL, NULL)$rhs$expr, expr)
})


test_that("can parse call sum over part of array", {
  res <- parse_expr(quote(a[] <- sum(x[, i])), NULL, NULL)
  expect_equal(
    res$rhs$expr,
    call("OdinReduce", "sum", "x", index = list(
      list(name = "i", type = "range", from = 1, to = quote(OdinDim("x", 1L))),
      list(name = "j", type = "single", at = quote(i)))))
  expect_equal(res$rhs$depends,
               list(functions = c("sum", "["), variables = "x"))
})


test_that("can parse call sum over part of part of array", {
  res <- parse_expr(quote(a[] <- sum(x[a:b, i])), NULL, NULL)
  expect_equal(
    res$rhs$expr,
    call("OdinReduce", "sum", "x", index = list(
      list(name = "i", type = "range", from = quote(a), to = quote(b)),
      list(name = "j", type = "single", at = quote(i)))))
  expect_equal(res$rhs$depends,
               list(functions = c("sum", "[", ":"),
                    variables = c("x", "a", "b")))
})


test_that("can check that sum has the right number of arguments", {
  expect_error(
    parse_expr(quote(a <- sum(x, 1)), NULL, NULL),
    "Invalid call to 'sum': incorrect number of arguments")
})


test_that("argument to quote must be symbol or array access", {
  expect_error(
    parse_expr(quote(a <- sum(x + y)), NULL, NULL),
    "Expected argument to 'sum' to be an array")
})

test_that("require that range access is simple", {
  expect_error(
    parse_expr(quote(y <- sum(x[a:b + c])), NULL, NULL),
    "Invalid use of range operator ':' within 'sum' call")
})


test_that("array dimensions cannot be stochastic", {
  expect_error(
    parse_expr(quote(dim(a) <- Poisson(2)), NULL, NULL),
    "Array extent cannot be stochastic")
  expect_error(
    parse_expr(quote(dim(a) <- 3 + a * Poisson(2)), NULL, NULL),
    "Array extent cannot be stochastic")
})


test_that("array dimensions cannot be determined by time", {
  expect_error(
    parse_expr(quote(dim(a) <- time), NULL, NULL),
    "Array extent cannot be determined by time")
  expect_error(
    parse_expr(quote(dim(a) <- 3 + a * time), NULL, NULL),
    "Array extent cannot be determined by time")
})


test_that("dim rhs must be simple", {
  expect_error(
    parse_expr(quote(dim(a) <- f(1)), NULL, NULL),
    "Invalid function used on rhs of 'dim()': 'f'",
    fixed = TRUE)
  expect_error(
    parse_expr(quote(dim(a) <- f(1 * a)), NULL, NULL),
    "Invalid functions used on rhs of 'dim()': 'f' and '*'",
    fixed = TRUE)
})


test_that("validate assignment into arrays", {
  expect_error(
    parse_expr(quote(1[] <- 1), NULL, NULL),
    "Expected a symbol on the lhs of array assignment")
  expect_error(
    parse_expr(quote(deriv(1[]) <- 1), NULL, NULL),
    "Expected a symbol within 'deriv()' on the lhs of array assignment",
    fixed = TRUE)
})
