test_that("can parse trivial system", {
  res <- odin_parse({
    initial(x) <- 0
    update(x) <- 0
  })
  expect_equal(res$time, "discrete")
  expect_equal(res$class, "odin")
  expect_mapequal(
    res$location,
    list(contents = list(variables = "x",
                         shared = character(),
                         internal = character(),
                         data = character(),
                         output = character(),
                         stack = character()),
         location = c(x = "state"),
         type = c(x = "real_type"),
         packing = list(state = list(x = 0))))
})
