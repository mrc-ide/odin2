test_that("detect very simple range access errors", {
  err <- expect_error(
    odin_parse({
      initial(a[]) <- 1
      update(a[]) <- sum(x)
      x[] <- a[i]
      dim(x) <- 4
      dim(a) <- 3
    }),
    "Out of range read of 'a' in 'a[i]'",
    fixed = TRUE)
})


test_that("detect range access errors via assignments", {
  err <- expect_error(
    odin_parse({
      initial(a[]) <- 1
      update(a[]) <- sum(x)
      x[] <- a[i]
      dim(x) <- n
      dim(a) <- m
      n <- 4
      m <- 3
    }),
    "Out of range read of 'a' in 'a[i]'",
    fixed = TRUE)

  err <- expect_error(
    odin_parse({
      initial(a[]) <- 1
      update(a[]) <- sum(x)
      x[] <- a[i]
      dim(x) <- n + 1
      dim(a) <- n
      n <- 4
    }),
    "Out of range read of 'a' in 'a[i]'",
    fixed = TRUE)
})

test_that("detect range access errors in loops over arrays", {
  expect_error(
    odin_parse({
      m[, ] <- x[i] * y[j]
      x <- parameter()
      y <- parameter()
      dim(m) <- c(2, 3)
      dim(x) <- 1
      dim(y) <- 2
      initial(a) <- 0
      update(a) <- sum(m)
    }),
    "Out of range read of 'x' in 'x[i]'",
    fixed = TRUE)
})


test_that("detect range access errors in matrices", {
  expect_error(
    odin_parse({
      m[, ] <- x[i, j]
      x <- parameter()
      dim(m) <- c(2, 3)
      dim(x) <- c(2, 2)
      initial(a) <- 0
      update(a) <- sum(m)
    }),
    "Out of range read of 'x' in dimension 2 of 'x[i, j]'",
    fixed = TRUE)
})


test_that("difficult constraints", {
  ## This would be an out of bounds access but we cannot easily see it
  ## because it involves both 'i' and 'j' in a single access (putting
  ## the if/else on the outside would work fine though)
  expect_warning(
    odin_parse({
      initial(a[, ]) <- 1
      update(a[, ]) <- x[if (time > 10) i else j]
      x[] <- Normal(0, 1)
      dim(x) <- 3
      dim(a) <- c(2, 2)
    }),
    "Cannot validate array access")
})


test_that("Can detect out of range write", {
  expect_error(
    odin_parse({
      initial(x) <- 0
      update(x) <- sum(a)
      dim(a) <- n + 1
      n <- parameter()
      a[n + 2] <- 1
    }),
    "Out of range write of 'a' in 'a[n + 2]'",
    fixed = TRUE)
})



test_that("can simplify constraints", {
  variables <- c("x", "y", "z")
  equations <- list(
    a = list(rhs = list(expr = 10)),
    p = list(special = "parameter"),
    q = list(special = "magic"))
  arrays <- list(
    v = list(5),
    m = list(2, 3))
  expect_equal(
    constraint_simplify_expr(1, arrays, equations, variables), 1)
  expect_equal(
    constraint_simplify_expr(quote(a), arrays, equations, variables),
    10)
  expect_equal(
    constraint_simplify_expr(quote(x), arrays, equations, variables),
    quote(OdinVariable("x")))
  expect_equal(
    constraint_simplify_expr(quote(a + 1), arrays, equations, variables),
    11)

  expect_equal(
    constraint_simplify_expr(quote(length(v)), arrays, equations, variables),
    5)
  expect_equal(
    constraint_simplify_expr(quote(nrow(m)), arrays, equations, variables),
    2)
  expect_equal(
    constraint_simplify_expr(quote(ncol(m)), arrays, equations, variables),
    3)
  expect_equal(
    constraint_simplify_expr(quote(OdinDim("m", 2)),
                             arrays, equations, variables),
    3)

  expect_equal(
    constraint_simplify_expr(quote(as.integer(1) + 2),
                             arrays, equations, variables),
    3)

  ## We have reached the bounds of knowledge here:
  expect_equal(
    constraint_simplify_expr(quote(p + 1), arrays, equations, variables),
    quote(1 + OdinParameter("p")))
  expect_equal(
    constraint_simplify_expr(quote(x + 1), arrays, equations, variables),
    quote(1 + OdinVariable("x")))

  ## Some bugs:
  expect_error(
    constraint_simplify_expr(NULL, arrays, equations, variables),
    "Unexpected value during simplification")
  expect_error(
    constraint_simplify_expr(quote(q), arrays, equations, variables),
    "Unexpected equation type during simplification")
})


test_that("can deparse constraints", {
  arrays <- list(x = vector("list", 1),
                 y = vector("list", 2),
                 z = vector("list", 3))

  expect_equal(constraint_deparse_rewrite(1, arrays), 1)
  expect_equal(constraint_deparse_rewrite(quote(a), arrays), quote(a))

  expect_equal(
    constraint_deparse_rewrite(quote(OdinParameter("a")), arrays),
    quote(a))
  expect_equal(
    constraint_deparse_rewrite(quote(OdinVariable("a")), arrays),
    quote(a))

  expect_equal(
    constraint_deparse_rewrite(quote(OdinDim("x", 1)), arrays),
    quote(length(x)))
  expect_equal(
    constraint_deparse_rewrite(quote(OdinDim("y", 1)), arrays),
    quote(nrow(y)))
  expect_equal(
    constraint_deparse_rewrite(quote(OdinDim("z", 1)), arrays),
    quote(dim(z)[[1]]))
})


test_that("solve simple constraints", {
  expect_equal(constraint_solve(1, 2, "max"), list(valid = TRUE))
  expect_equal(constraint_solve(2, 2, "max"), list(valid = TRUE))
  expect_equal(constraint_solve(2, 1, "max"), list(valid = FALSE))
  expect_equal(constraint_solve(quote(a), quote(a), "max"),
               list(valid = TRUE))
  expect_equal(constraint_solve(quote(a + b), quote(a + b), "max"),
               list(valid = TRUE))

  expect_equal(constraint_solve(quote(a - 1), quote(a), "max"),
               list(valid = TRUE))
  expect_equal(constraint_solve(quote(a + 1), quote(a), "max"),
               list(valid = FALSE))

  expect_equal(
    constraint_solve(quote(OdinParameter("a") - 1),
                     quote(OdinParameter("a")),
                     "max"),
    list(valid = TRUE))
  expect_equal(
    constraint_solve(quote(OdinParameter("a") + 1),
                     quote(OdinParameter("a")),
                     "max"),
    list(valid = FALSE))
  expect_equal(
    constraint_solve(quote(OdinParameter("a")),
                     quote(OdinParameter("b")),
                     "max"),
    list(valid = NA,
         constraint = quote(OdinParameter("b") >= OdinParameter("a"))))
})


test_that("tidy expression", {
  expect_equal(constraint_tidy(quote(a + 1), "max"), quote(a + 1 >= 0))
  expect_equal(constraint_tidy(quote(-a - 1 + OdinParameter("x")), "max"),
               quote(OdinParameter("x") >= 1 + a))
})


test_that("can detect out-of-range access in sum()", {
  expect_error(
    odin_parse({
      N[] <- 0
      dim(N) <- 3
      update(x) <- a
      initial(x) <- 0
      a <- sum(N[5])
    }),
    "Out of range read of 'N' in 'N[5]'",
    fixed = TRUE)

  expect_error(
    odin_parse({
      N[] <- 0
      dim(N) <- 3
      update(x) <- a
      initial(x) <- 0
      a <- sum(N[1:5])
    }),
    "Out of range read of 'N' in 'N[1:5]'",
    fixed = TRUE)
})


test_that("difficult constraints in sum expressions", {
  ## This would be an out of bounds access but we cannot easily see it
  ## because it involves both 'i' and 'j' in a single access (putting
  ## the if/else on the outside would work fine though)
  expect_warning(
    odin_parse({
      initial(a[, ]) <- 1
      update(a[, ]) <- sum(x[1:(if (time > 10) i else j)])
      x[] <- Normal(0, 1)
      dim(x) <- 3
      dim(a) <- c(2, 2)
    }),
    "Cannot validate array access")
})


test_that("can identify trivial negative array access", {
  err <- expect_error(
    odin_parse({
      initial(a) <- 1
      update(a) <- b[-2]
      dim(b) <- 3
      b <- parameter()
    }),
    "Out of range read of 'b' in 'b[-2]'",
    fixed = TRUE)
  expect_equal(
    err$body,
    c(i = "Attempting to read before the start of 'b'",
      x = "Trying to read element: -2"))
})


test_that("can identify simple negative array access", {
  err <- expect_error(
    odin_parse({
      initial(a) <- 1
      update(a) <- b[v - 2]
      v <- 1
      dim(b) <- 3
      b <- parameter()
    }),
    "Out of range read of 'b' in 'b[v - 2]'",
    fixed = TRUE)
  expect_equal(
    err$body,
    c(i = "Attempting to read before the start of 'b'",
      x = "Trying to read element: -1"))
})


test_that("can identify simple negative matrix access", {
  err <- expect_error(
    odin_parse({
      initial(a) <- 1
      update(a) <- b[v - 2, 2]
      v <- 1
      dim(b) <- c(3, 2)
      b <- parameter()
    }),
    "Out of range read of 'b' in dimension 1 of 'b[v - 2, 2]'",
    fixed = TRUE)
  expect_equal(
    err$body,
    c(i = "Attempting to read before the start of dimension 1 of 'b'",
      x = "Trying to read element: -1"))
})


test_that("can prevent writing to negative index", {
  expect_error(
    odin_parse({
      initial(a) <- 1
      update(a) <- b[1]
      dim(b) <- 4
      b[length(b) - 5] <- 1
    }),
    "Out of range write of 'b' in 'b[length(b) - 5]'",
    fixed = TRUE)
})


test_that("warn on error", {
  w <- testthat::capture_warnings(
    odin_parse({
      initial(a[]) <- 1
      update(a[]) <- sum(x)
      x[] <- a[i]
      dim(x) <- 4
      dim(a) <- 3
    }, check_bounds = "warning"))
  expect_length(w, 1)
  expect_match(w, "Out of range read of 'a' in 'a[i]'",
               fixed = TRUE)
})


test_that("warn on all access errors", {
  w <- testthat::capture_warnings(
    odin_parse({
      initial(a[]) <- 1
      update(a[]) <- sum(x)
      x[] <- a[i]
      x[5] <- 1
      dim(x) <- 4
      dim(a) <- 3
    }, check_bounds = "warning"))
  expect_length(w, 2)
  expect_match(w, "Out of range read of 'a' in 'a[i]'",
               fixed = TRUE, all = FALSE)
  expect_match(w, "Out of range write of 'x' in 'x[5]'",
               fixed = TRUE, all = FALSE)
})


test_that("can disable bounds checks", {
  expect_no_condition(
    odin_parse({
      initial(a[]) <- 1
      update(a[]) <- sum(x)
      x[] <- a[i]
      dim(x) <- 4
      dim(a) <- 3
    }, check_bounds = "disabled"))
})


test_that("select appropriate check_bounds mode", {
  withr::with_options(list(odin2.check_bounds = NULL), {
    expect_equal(odin_check_bounds_value(NULL, NULL), "error")
    expect_equal(odin_check_bounds_value("warning", NULL), "warning")
    expect_equal(odin_check_bounds_value("disabled", NULL), "disabled")
  })
  withr::with_options(list(odin2.check_bounds = "disabled"), {
    expect_equal(odin_check_bounds_value(NULL, NULL), "disabled")
    expect_equal(odin_check_bounds_value("warning", NULL), "warning")
    expect_equal(odin_check_bounds_value("error", NULL), "error")
  })
})


test_that("sensible error message if bounds checking fails", {
  skip_if_not_installed("mockery")
  mock_parse_array_bounds <- mockery::mock(stop("random error"))
  testthat::local_mocked_bindings(
    parse_array_bounds = mock_parse_array_bounds)
  expect_error(
    odin_parse({
      initial(x) <- 1
      update(x) <- 1
    }),
    "An odin bug is preventing analysis of your bounds")
})


test_that("skip over bounds checking involving time", {
  ## This failed prior to 0.3.35 (mrc-6625)
  expect_no_error(
    odin_parse({
      initial(x) <- 0
      update(x) <- a[min(time, 10)]
      dim(a) <- 10
      a[] <- Normal(0, 1)
    }))
})
