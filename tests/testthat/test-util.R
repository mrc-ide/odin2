test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
})


test_that("match_value", {
  expect_error(match_value("foo", letters), "must be one of")
  expect_silent(match_value("a", letters))
})


test_that("can match a simple call", {
  fn <- function(foo, bar = 1) NULL
  expect_equal(
    match_call(quote(f(a)), fn),
    list(success = TRUE, value = quote(f(foo = a, bar = 1))))
  expect_equal(
    match_call(quote(f(a, b)), fn),
    list(success = TRUE, value = quote(f(foo = a, bar = b))))
  expect_equal(
    match_call(quote(f(bar = 2, foo = 1)), fn),
    list(success = TRUE, value = quote(f(foo = 1, bar = 2))))

  res <- match_call(quote(f(baz = 1)), fn)
  expect_false(res$success)
})


test_that("prevent passing arguments by name", {
  fn1 <- function(.a, b) NULL
  fn2 <- function(.a, .b, c, d) NULL
  expect_equal(
    match_call(quote(f(1, 2)), fn1),
    list(success = TRUE, value = quote(f(a = 1, b = 2))))

  res <- match_call(quote(f(a = 1, 2)), fn1)
  expect_false(res$success)
  expect_match(conditionMessage(res$error),
               "Expected the first 1 argument to be unnamed")

  res <- match_call(quote(f(a = 1, 2)), fn2)
  expect_false(res$success)
  expect_match(conditionMessage(res$error),
               "Expected the first 2 arguments to be unnamed")

  res <- match_call(quote(f(c = 3, d = 4, 1, 2)), fn2)
  expect_false(res$success)
  expect_match(conditionMessage(res$error),
               "Expected the first 2 arguments to be unnamed")

  res <- match_call(quote(f(3, 4, a = 1, b = 2)), fn2)
  expect_false(res$success)
  expect_match(conditionMessage(res$error),
               "Expected arguments 'a' and 'b' to be unnamed")
})


test_that("prevent partial matching of arguments", {
  withr::local_options(warnPartialMatchArgs = FALSE)
  res <- match_call(quote(f(my = 1)), function(my_arg) NULL)
  expect_false(res$success)
  expect_match(
    conditionMessage(res$error),
    "Argument was expanded by partial matching: 'my' => 'my_arg'")

  res <- match_call(quote(f(my = 1, o = 2)), function(my_arg, other) NULL)
  expect_false(res$success)
  expect_match(
    conditionMessage(res$error),
    "Arguments were expanded by partial matching: 'my' => 'my_arg' and 'o' =>")

  res <- match_call(quote(f(my_arg = 1, o = 2)), function(my_arg, other) NULL)
  expect_false(res$success)
  expect_match(
    conditionMessage(res$error),
    "Argument was expanded by partial matching: 'o' => 'other'")
})


test_that("set_names copes with common pathologies", {
  expect_equal(set_names(character(), "x"),
               structure(character(), names = character()))
  expect_equal(set_names("a", "x"),
               c("x" = "a"))
  expect_equal(set_names(c("a", "b"), "x"),
               c("x" = "a", x = "b"))
  expect_equal(set_names(c("a", "b"), c("x", "y")),
               c("x" = "a", y = "b"))
  expect_null(set_names(NULL, "x"))
})
