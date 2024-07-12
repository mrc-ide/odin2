test_that("assert_scalar", {
  expect_error(assert_scalar(NULL), "must be a scalar")
  expect_error(assert_scalar(numeric(0)), "must be a scalar")
  expect_error(assert_scalar(1:2), "must be a scalar")
})


test_that("assert_character", {
  expect_silent(assert_character("a"))
  expect_error(assert_character(1), "Expected '.+' to be character")
  expect_error(assert_character(TRUE), "Expected '.+' to be character")
})


test_that("assert_logical", {
  expect_silent(assert_logical(TRUE))
  expect_error(assert_logical(1), "Expected '.+' to be logical")
  expect_error(assert_logical("true"), "Expected '.+' to be logical")
})


test_that("assert_nonmissing", {
  expect_silent(assert_nonmissing(1))
  expect_error(assert_nonmissing(NA), "Expected '.+' to be non-NA")
})
