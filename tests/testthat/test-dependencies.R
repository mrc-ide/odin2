test_that("can solve dependencies for trivial system", {
  expect_equal(topological_order(list()),
               list(success = TRUE, value = integer()))
  expect_equal(topological_order(list(a = character())),
               list(success = TRUE, value = 1))
  expect_equal(topological_order(list(a = character(), b = character())),
               list(success = TRUE, value = 1:2))
})


test_that("can solve dependencies for simple system", {
  deps <- list(a = character(), b = "a", c = "b")
  expect_equal(topological_order(deps),
               list(success = TRUE, value = 1:3))
  deps <- list(a = character(), b = "a", c = "b")
  expect_equal(topological_order(rev(deps)),
               list(success = TRUE, value = 3:1))
})


test_that("can report back on cyclic dependencies", {
  deps <- list(x = character(), y = "x", a = "c", b = "a", c = "a")
  expect_equal(topological_order(deps),
               list(success = FALSE, error = 3:5))
})
