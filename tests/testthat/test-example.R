test_that("can create example package", {
  path <- example_package()
  expect_true(file.exists(file.path(path, "DESCRIPTION")))
  expect_true(file.exists(file.path(path, "inst/odin/sir.R")))
})
