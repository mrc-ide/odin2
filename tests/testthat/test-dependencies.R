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


test_that("can join dependencies", {
  deps <- list(
    list(functions = c("a", "f", "g"), variables = c("a", "b", "c")),
    list(functions = "f",              variables = c("c", "d", "e")),
    list(functions = c("h", "i"),      variables = character()))
  expect_equal(
    join_dependencies(list()),
    list(functions = character(), variables = character()))
  expect_equal(
    join_dependencies(deps[1]),
    deps[[1]])
  expect_equal(
    join_dependencies(deps),
    list(functions = c("a", "f", "g", "h", "i"),
         variables = c("a", "b", "c", "d", "e")))
})


test_that("cope with poorly deparsing epxressions", {
  expr <- quote(
    OdinStochasticCall(
      sample = "binomial",
      mean = S_leave[i, j, k] * (S_rates[i, j, k, 1] / S_leave_rate[i, j, k])
    )(S_leave[i, j, k], S_rates[i, j, k, 1]/S_leave_rate[i, j, k]))
  res <- find_dependencies(expr)
  expect_setequal(res$functions, c("[", "/"))
  expect_setequal(
    res$variables,
    c("S_leave", "i", "j", "k", "S_rates", "S_leave_rate"))
})
