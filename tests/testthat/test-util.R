test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
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

  ## Partial matching still enabled
  expect_equal(
    suppressWarnings(match_call(quote(f(fo = 2)), fn)),
    list(success = TRUE, value = quote(f(foo = 2, bar = 1))))
})
