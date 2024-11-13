test_that("basic manipulation", {
  expect_equal(expr_to_sum_of_parts(1), list(1))
  expect_equal(expr_to_sum_of_parts(quote(x)), list(quote(x)))
  expect_equal(expr_to_sum_of_parts(quote(x + y)), list(quote(x), quote(y)))
  expect_equal(expr_to_sum_of_parts(quote(x + y + z)),
               list(quote(x), quote(y), quote(z)))
  expect_equal(expr_to_sum_of_parts(quote(x + 2 * y + z)),
               list(quote(x), quote(2 * y), quote(z)))
  expect_equal(expr_to_sum_of_parts(quote(x - y)), list(quote(x), quote(-y)))
  expect_equal(expr_to_sum_of_parts(quote(x - y - z)),
               list(quote(x), quote(-y), quote(-z)))
})
