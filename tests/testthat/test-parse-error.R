test_that("can explain an error", {
  skip_if_not_installed("mockery")
  mock_explain <- mockery::mock()
  mockery::stub(odin_error_explain, "error_explain", mock_explain)
  odin_error_explain("E1001")
  mockery::expect_called(mock_explain, 1)
  expect_equal(
    mockery::mock_args(mock_explain)[[1]],
    list(errors, "E1001", "pretty"))
})
