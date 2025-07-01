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


test_that("can invert expressions", {
  expect_false(expr_not(TRUE))
  expect_true(expr_not(FALSE))

  expect_equal(expr_not(quote(!a)), quote(a))
  expect_equal(expr_not(quote(!(a + b))), quote(a + b))

  expect_equal(expr_not(quote(a == b)), quote(a != b))
  expect_equal(expr_not(quote(a != b)), quote(a == b))
  expect_equal(expr_not(quote(a < b)), quote(a >= b))
  expect_equal(expr_not(quote(a > b)), quote(a <= b))
  expect_equal(expr_not(quote(a <= b)), quote(a > b))
  expect_equal(expr_not(quote(a >= b)), quote(a < b))

  expect_equal(expr_not(quote(a)), quote(!a))
  expect_equal(expr_not(quote(a + b)), quote(!(a + b)))
})


test_that("can fold expressions", {
  expect_identical(expr_fold(list(quote(a)), "+"), quote(a))
  expect_identical(expr_fold(list(quote(a), quote(b)), "+"), quote(a + b))
  expect_identical(expr_fold(list(quote(a), quote(b), quote(c)), "+"),
                   quote(a + b + c))
})
