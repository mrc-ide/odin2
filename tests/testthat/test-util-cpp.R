test_that("can create simple C++ functions", {
  expect_equal(
    cpp_function("a", "b", list("int" = "x"), "return x;"),
    c("a b(int x) {",
      "  return x;",
      "}"))
  expect_equal(
    cpp_function("a", "b", list("int" = "x"), "return x;", static = TRUE),
    c("static a b(int x) {",
      "  return x;",
      "}"))
  expect_equal(
    cpp_function("a", "b", list("int" = "x"), character()),
    c("a b(int x) {",
      "}"))
})
