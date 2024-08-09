test_that("can parse simple assignments", {
  res <- parse_expr(quote(a <- 1), NULL, NULL)
  expect_equal(res$lhs$name, "a")
  expect_equal(res$rhs$type, "expression")
  expect_equal(res$rhs$expr, 1)
  expect_equal(res$rhs$depends,
               list(functions = character(), variables = character()))
})


test_that("can parse simple expressions involving functions/variables", {
  res <- parse_expr(quote(a <- b + c / b), NULL, NULL)
  expect_equal(res$rhs$expr, quote(b + c / b))
  expect_equal(res$rhs$depends,
               list(functions = c("+", "/"), variables = c("b", "c")))
})


test_that("require that assignment lhs is reasonable", {
  expect_error(
    parse_expr(quote(1 <- 1), NULL, NULL),
    "Expected a symbol on the lhs")
  expect_error(
    parse_expr(quote(f(1) <- 1), NULL, NULL),
    "Expected a symbol on the lhs")
})


## Special calls are initial/deriv/update/dim/output/config/compare
test_that("allow calls on lhs", {
  res <- parse_expr(quote(initial(x) <- 1), NULL, NULL)
  expect_equal(res$lhs$name, "x")
  expect_equal(res$special, "initial")
  expect_equal(res$rhs$expr, 1)
  expect_equal(parse_expr(quote(deriv(x) <- 1), NULL, NULL)$special, "deriv")
  expect_equal(parse_expr(quote(update(x) <- 1), NULL, NULL)$special, "update")
})


test_that("require that special calls are (currently) simple", {
  expect_error(
    parse_expr(quote(update(x, TRUE) <- 1), NULL, NULL),
    "Invalid special function call")
  expect_error(
    parse_expr(quote(initial() <- 1), NULL, NULL),
    "Invalid special function call")
  err <- expect_error(
    parse_expr(quote(initial(x = 1) <- 1), NULL, NULL),
    "Invalid special function call")
})


test_that("can parse parameter definitions", {
  res <- parse_expr(quote(a <- parameter()), NULL, NULL)
  expect_equal(res$rhs$type, "parameter")
  expect_null(res$rhs$args$default)
  expect_equal(res$rhs$args$constant, NA)
  expect_false(res$rhs$args$differentiate)
})


test_that("can parse parameter definitions with defaults", {
  res <- parse_expr(quote(a <- parameter(10)), NULL, NULL)
  expect_equal(res$rhs$type, "parameter")
  expect_equal(res$rhs$args$default, 10)
  expect_equal(res$rhs$args$constant, NA)
  expect_false(res$rhs$args$differentiate)
})


test_that("can parse parameter definitions with expression defaults", {
  res <- parse_expr(quote(a <- parameter(4 / 3)), NULL, NULL)
  expect_equal(res$rhs$type, "parameter")
  expect_equal(res$rhs$args$default, quote(4 / 3))
  expect_equal(res$rhs$args$constant, NA)
  expect_false(res$rhs$args$differentiate)
})


test_that("parameter defaults must be simple", {
  expect_error(
    parse_expr(quote(a <- parameter(a)), NULL, NULL),
    "Invalid default argument to 'parameter()': a",
    fixed = TRUE)
})


test_that("validate differentiate argument", {
  res <- parse_expr(quote(a <- parameter(differentiate = TRUE)), NULL, NULL)
  expect_true(res$rhs$args$differentiate)
  res <- parse_expr(quote(a <- parameter()), NULL, NULL)
  expect_false(res$rhs$args$differentiate)

  expect_error(
    parse_expr(quote(a <- parameter(differentiate = x)), NULL, NULL),
    "'differentiate' must be a scalar logical, but was 'x'")
  expect_error(
    parse_expr(quote(a <- parameter(differentiate = NA)), NULL, NULL),
    "'differentiate' must be a scalar logical, but was 'NA'")
  expect_error(
    parse_expr(quote(a <- parameter(differentiate = NULL)), NULL, NULL),
    "'differentiate' must be a scalar logical, but was 'NULL'")
})


test_that("validate constant argument", {
  res <- parse_expr(quote(a <- parameter(constant = TRUE)), NULL, NULL)
  expect_true(res$rhs$args$constant)
  res <- parse_expr(quote(a <- parameter(constant = FALSE)), NULL, NULL)
  expect_false(res$rhs$args$constant)
  res <- parse_expr(quote(a <- parameter()), NULL, NULL)
  expect_equal(res$rhs$args$constant, NA)

  expect_error(
    parse_expr(quote(a <- parameter(constant = x)), NULL, NULL),
    "'constant' must be a scalar logical if given, but was 'x'")
  expect_error(
    parse_expr(quote(a <- parameter(constant = NA)), NULL, NULL),
    "'constant' must be a scalar logical if given, but was 'NA'")
})


test_that("can't use both constant and differentiate", {
  expect_error(
    parse_expr(quote(a <- parameter(differentiate = TRUE, constant = TRUE)),
               NULL, NULL),
    "Differentiable parameters must not be constant")
})


test_that("sensible error if parameters are incorrectly specified", {
  expect_error(
    parse_expr(quote(a <- parameter(other = TRUE)), NULL, NULL),
    "Invalid call to 'parameter()'",
    fixed = TRUE)
})


test_that("parse data assignment", {
  res <- parse_expr(quote(d <- data()), NULL, NULL)
  expect_equal(res$special, "data")
  expect_equal(res$lhs$name, "d")
  expect_equal(res$rhs, list(type = "data"))
})


test_that("data calls must be very simple", {
  expect_error(
    parse_expr(quote(d <- data(integer = TRUE)), NULL, NULL),
    "Calls to 'data()' must have no arguments",
    fixed = TRUE)
  expect_error(
    parse_expr(quote(deriv(d) <- data()), NULL, NULL),
    "Calls to 'data()' must be assigned to a symbol",
    fixed = TRUE)
})


test_that("parameter calls must be assigned to a symbol", {
  expect_error(
    parse_expr(quote(deriv(d) <- parameter()), NULL, NULL),
    "Calls to 'parameter()' must be assigned to a symbol",
    fixed = TRUE)
})


test_that("print not yet supported", {
  expect_error(
    parse_expr(quote(print(x)), NULL, NULL),
    "'print()' is not implemented yet",
    fixed = TRUE)
})


test_that("delays not yet supported", {
  expect_error(
    parse_expr(quote(a <- delay(b, 1)), NULL, NULL),
    "'delay()' is not implemented yet",
    fixed = TRUE)
})


test_that("delays not yet supported", {
  expect_error(
    parse_expr(quote(a <- interpolate(b, "constant")), NULL, NULL),
    "'interpolate()' is not implemented yet",
    fixed = TRUE)
})


test_that("arrays not yet supported", {
  expect_error(
    parse_expr(quote(a[] <- 1), NULL, NULL),
    "Arrays are not supported yet",
    fixed = TRUE)
})


test_that("Reject unclassifiable expressions", {
  expect_error(
    parse_expr(quote(a), NULL, NULL),
    "Unclassifiable expression")
})


test_that("can parse expressions that involve stochastics", {
  res <- parse_expr(quote(a <- Normal(0, 1)), NULL, NULL)
  expect_null(res$special)
  expect_equal(res$lhs, list(name = "a"))
  expect_identical(
    res$rhs$expr,
    quote(OdinStochasticCall(sample = "normal",
                             density = "normal",
                             mean = 0)(0, 1)))
  expect_equal(res$rhs$depends,
               list(functions = "Normal", variables = character()))
})


test_that("can parse compound expressions that involve stochastics", {
  res <- parse_expr(quote(a <- Normal(0, 1) + Binomial(n, p)), NULL, NULL)

  expect_identical(
    res$rhs$expr[[2]],
    quote(OdinStochasticCall(sample = "normal",
                             density = "normal",
                             mean = 0)(0, 1)))
  expect_identical(
    res$rhs$expr[[3]],
    quote(OdinStochasticCall(sample = "binomial",
                             density = NULL,
                             mean = n * p)(n, p)))
  expect_equal(res$rhs$depends,
               list(functions = c("+", "Normal", "Binomial"),
                    variables = c("n", "p")))
  expect_true(res$rhs$is_stochastic)

  expect_identical(rewrite_stochastic_to_expectation(res$rhs$expr),
                   quote(0 + n * p))
})


test_that("can parse recursive expressions that involve stochastics", {
  res <- parse_expr(quote(a <- Normal(Poisson(p), Exponential(r))),
                    NULL, NULL)
  expect_equal(res$rhs$expr[[1]]$mean, quote(p))

  res <- parse_expr(quote(a <- Normal(Poisson(Exponential(r)), 1)),
                    NULL, NULL)
  expect_equal(res$rhs$expr[[1]]$mean, quote(1 / r))
})


test_that("throw sensible error if stochastic parse fails", {
  expect_error(
    parse_expr(quote(a <- Normal(mu = 0, sigma = 1)), NULL, NULL),
    "Invalid call to 'Normal()'", fixed = TRUE)
})
