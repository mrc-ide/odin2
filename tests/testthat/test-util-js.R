test_that("can create simple js functions", {
  expect_equal(
    js_method("a", c("x", "y"), "return x + y;"),
    c("a(x, y) {",
      "  return x + y;",
      "},")
  )
})
