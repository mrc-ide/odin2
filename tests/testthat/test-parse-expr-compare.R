test_that("Can parse compare expression", {
  res <- parse_expr(quote(x ~ Normal(0, 1)), NULL, NULL)
  expect_equal(res$rhs$type, "compare")
  expect_equal(res$rhs$density$cpp, "normal")
  expect_equal(res$rhs$args, list(quote(x), 0, 1))
  expect_equal(res$rhs$depends,
               list(functions = "Normal", variables = "x"))
})


test_that("Suitable lhs and rhs on a '~' comparison", {
  expect_error(
    parse_expr(quote(initial(x) ~ 1), NULL, NULL),
    "Invalid special function 'initial()' on the lhs of a `~` comparison",
    fixed = TRUE)
  expect_error(
    parse_expr(quote(f(x)[] ~ 1), NULL, NULL),
    "Invalid special function 'f()' on the lhs of array assignment",
    fixed = TRUE)
  expect_error(
    parse_expr(quote(x ~ 1), NULL, NULL),
    "The rhs of '~' is not a function call",
    fixed = TRUE)
})


test_that("comparison calls must wrap symbols", {
  expect_error(
    parse_expr(quote(1 ~ Normal(0, 1)), NULL, NULL),
    "Invalid target '1' on the lhs of a `~` comparison",
    fixed = TRUE)
  expect_error(
    parse_expr(quote(f(x) ~ Normal(0, 1)), NULL, NULL),
    "Invalid special function 'f()' on the lhs of a `~` comparison",
    fixed = TRUE)
})


test_that("can process arrays within compare calls", {
  res <- parse_expr(quote(x[] ~ Normal(0, 1)), NULL, NULL)
  expect_null(res$lhs)
  expect_equal(res$array,
               list(list(name = "i",
                         type = "range",
                         from = 1,
                         to = quote(OdinDim("x", 1L)))))
})


test_that("parse compare call rhs as distributions", {
  expect_error(
    parse_expr(quote(x ~ 1), NULL, NULL),
    "The rhs of '~' is not a function call")
  expect_error(
    parse_expr(quote(x ~ Foo(0, 1)), NULL, NULL),
    "Unknown distribution 'Foo'")
  expect_error(
    parse_expr(quote(x ~ Normal(mu = 1)), NULL, NULL),
    "Invalid call to 'Normal()'",
    fixed = TRUE)
})


test_that("can apply transformation to data before use", {
  res <- odin_parse({
    initial(x) <- 1
    update(x) <- 1
    d <- data()
    d2 <- d * d
    d2 ~ Normal(x, 1)
  })
  expect_equal(res$data, data_frame(name = "d"))
  expect_equal(res$phases$compare$equations, "d2")
  expect_equal(res$storage$location[["d2"]], "stack")
})


test_that("data can only be used within the compare phase", {
  expect_error(
    odin_parse({
      initial(x) <- 1
      update(x) <- d
      d <- data()
    }),
    "Data may only be referenced from equations used in comparison")

  expect_error(
    odin_parse({
      initial(x) <- 1
      update(x) <- a
      a <- d * d
      d <- data()
    }),
    "Data may only be referenced from equations used in comparison")
})


test_that("compare expressions use correct array index", {
  expect_error(
    parse_expr(quote(x[] ~ Normal(a[j], 1)), NULL, NULL),
    "Invalid index access used on rhs of equation: 'j'")
})
