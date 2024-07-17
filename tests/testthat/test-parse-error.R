test_that("can explain an error", {
  skip_if_not_installed("mockery")
  mock_browse <- mockery::mock()
  mockery::stub(odin_error_explain, "utils::browseURL", mock_browse)
  odin_error_explain("E0001")
  mockery::expect_called(mock_browse, 1)
  expect_equal(
    mockery::mock_args(mock_browse)[[1]],
    list("https://mrc-ide.github.io/odin2/articles/errors.html#e0001"))
})


test_that("error if given invalid code", {
  msg <- "Invalid code 'E001', should match 'Exxxx'"
  expect_error(odin_error_explain("E001"),
               "Invalid code 'E001', should match 'Exxxx'")
  expect_error(odin_error_explain("e0001"),
               "Invalid code 'e0001', should match 'Exxxx'")
  expect_error(odin_error_explain("E00001"),
               "Invalid code 'E00001', should match 'Exxxx'")
  expect_error(odin_error_explain("anything"),
               "Invalid code 'anything', should match 'Exxxx'")
})


test_that("error if given unknown code", {
  expect_error(
    odin_error_explain("E9999"),
    "Error 'E9999' is undocumented")
})
