test_that("can translate user()", {
  d <- odin_parse({
    a <- user(1)
    initial(x) <- 0
    update(x) <- x + a
  }, compatibility = "silent")
  expect_equal(d$equations$a$src$value,
               quote(a <- parameter(1)))
  expect_equal(d$equations$a$src$compat,
               list(type = "user", original = quote(a <- user(1))))
})


test_that("can control severity of reporting", {
  code <- "a <- user(1)\ninitial(x) <- 0\nupdate(x) <- x + a"
  expect_silent(odin_parse(code, compatibility = "silent"))
  w <- expect_warning(
    odin_parse(code, compatibility = "warning"),
    "Found 1 compatibility issue")

  e <- expect_error(
    odin_parse(code, compatibility = "error"),
    "Found 1 compatibility issue")

  expect_true(startsWith(conditionMessage(e), conditionMessage(w)))
  expect_equal(
    e$body[2],
    c(x = "a <- user(1)"))
  expect_equal(
    e$body[3],
    c(v = "a <- parameter(1)"))
})


test_that("can translate simple user calls", {
  expect_equal(
    parse_compat_fix_user(list(value = quote(a <- user()), index = 1L)),
    list(value = quote(a <- parameter()),
         index = 1L,
         compat = list(type = "user", original = quote(a <- user()))))
  expect_equal(
    parse_compat_fix_user(list(value = quote(a <- user(1)), index = 1L)),
    list(value = quote(a <- parameter(1)),
         index = 1L,
         compat = list(type = "user", original = quote(a <- user(1)))))
  expect_equal(
    parse_compat_fix_user(list(value = quote(a <- user(min = 0)), index = 1L)),
    list(value = quote(a <- parameter(min = 0)),
         index = 1L,
         compat = list(type = "user", original = quote(a <- user(min = 0)))))
  expect_equal(
    parse_compat_fix_user(list(value = quote(a <- user(max = 1)), index = 1L)),
    list(value = quote(a <- parameter(max = 1)),
         index = 1L,
         compat = list(type = "user", original = quote(a <- user(max = 1)))))
})


test_that("handle errors that occur in translated code", {
  err <- expect_error(
    odin_parse({
      a <- user(sqrt(b))
      b <- 1
      initial(x) <- 0
      update(x) <- x + a
    }, compatibility = "silent",),
    "Invalid default argument to 'parameter()': sqrt(b)",
    fixed = TRUE)

  expect_match(
    conditionMessage(err),
    "The expression above has been translated")
})


test_that("handle failure to pass a user call", {
  expect_error(
    odin_parse({
      a <- user(min = 1, min = 1)
      initial(x) <- 0
      update(x) <- x + a
    }),
    "Failed to translate your 'user()' expression to use 'parameter()'",
    fixed = TRUE)
})


test_that("can translate simple calls to distributions", {
  expect_warning(
    res <- odin_parse({
      initial(a) <- 1
      update(a) <- rnorm(a, 1)
    }),
    "Found 1 compatibility issue")
  expect_equal(
    res$phases$update$variables[[1]]$rhs$expr,
    quote(OdinStochasticCall(sample = "normal", mean = a)(a, 1)))
})


test_that("can translate parameter assignment using array access", {
  expect_warning(
    res <- odin_parse({
      initial(y) <- 1
      update(y) <- Normal(a[1], a[2])
      a[] <- parameter()
      dim(a) <- 2
    }),
    "Found 1 compatibility issue")
  expect_equal(
    res$equations$a$src$value,
    quote(a <- parameter()))
})


test_that("can cope with two compatibility issues in one line", {
  w <- expect_warning(
    res <- odin_parse({
      initial(y) <- 1
      update(y) <- a[1] + a[1]
      a[] <- user()
      dim(a) <- 2
    }),
    "Found 2 compatibility issues")
  expect_match(conditionMessage(w),
               "Replace calls to 'user()' with 'parameter()'",
               fixed = TRUE)
  expect_match(conditionMessage(w),
               "Drop arrays from lhs of assignments from 'parameter()'",
               fixed = TRUE)
  expect_equal(
    res$equations$a$src$value,
    quote(a <- parameter()))
})

test_that("can translate compare()", {
  expect_warning(
    res <- odin_parse({
      update(x) <- 1
      initial(x) <- 1
      d <- data()
      compare(d) ~ Normal(x, 1)
    }), "Found 1 compatibility issue")

  expect_equal(
    res$phases$compare$compare[[1]]$src$value[[2]], quote(d))
})


test_that("can translate user(integer = TRUE)", {
  expect_warning(
    res <- odin_parse({
      update(x) <- a
      initial(x) <- 1
      a <- user(integer = TRUE)
    }),
    "Found 1 compatibility issue")
  expect_equal(
    res$equations$a$src$value,
    quote(a <- parameter(type = "integer")))
})


test_that("translate use of 't' into 'time' on use", {
  expect_warning(
    res <- odin_parse({
      deriv(x) <- t
      initial(x) <- 0
    }),
    "Found 1 compatibility issue")
  expect_identical(
    res$phases$deriv$variables[[1]]$rhs$expr,
    quote(time))
})


test_that("fix assignment to time", {
  expect_warning(
    d <- odin_parse({
      update(x) <- x + sqrt(time) / a
      initial(x) <- 0
      time <- dt * step
      a <- 10
    }),
    "Found 1 compatibility issue")
  expect_equal(names(d$equations), "a")
})


test_that("error on unfixable time assignment", {
  err <- expect_error(
    odin_parse({
      update(x) <- x + sqrt(time)
      initial(x) <- 0
      time <- ((dt * step))
    }),
    "Don't assign to 'time'")
})


test_that("fix assignment to dt", {
  expect_warning(
    d <- odin_parse({
      update(x) <- x + a * dt
      initial(x) <- 0
      dt <- parameter(0.5)
      a <- 10
    }),
    "Found 1 compatibility issue")
  expect_equal(names(d$equations), "a")
})


test_that("error on unfixable dt assignment", {
  expect_error(
    odin_parse({
      update(x) <- x + a * dt
      initial(x) <- 0
      dt <- 0.5
      a <- 10
    }),
    "Don't assign to 'dt'")
})


test_that("fix use of 't' rather than 'time'", {
  w <- expect_warning(
    d <- odin_parse({
      update(x) <- x + a * t
      initial(x) <- 0
      a <- 10
    }),
    "Found 1 compatibility issue")
  expect_match(
    conditionMessage(w),
    "Use 'time' and not 't' to refer to time")
  expect_equal(d$phases$update$variables[[1]]$src$value,
               quote(update(x) <- x + a * time))
})


test_that("Fix use of 'step' in equations", {
  expect_error(
    odin_parse({
      update(x) <- x + a * 2
      initial(x) <- 0
      a <- if (step %% 10 == 0) 0 else 1
    }),
    "Use of 'step' is no longer allowed")
})


test_that("fix user-sized arrays", {
  expect_warning(
    res <- odin_parse({
      a[, ] <- user()
      dim(a) <- user()
      update(x) <- x + sum(a)
      initial(x) <- 0
    }),
    "Found 4 compatibility issues")
  expect_equal(res$equations$dim_a$src$value,
               quote(dim(a) <- parameter(rank = 2)))
  expect_equal(res$storage$arrays$rank, 2)
})


test_that("error where we can't determine rank in migration", {
  expect_error(
    odin_parse({
      dim(a) <- user()
      update(x) <- x + sum(a)
      initial(x) <- 0
    }),
    "Can't determine rank for 'dim() <- user()' call",
    fixed = TRUE)
})


test_that("disallow parsing interpolation to slice", {
  w <- expect_warning(
    odin_parse({
      a <- parameter()
      dim(a) <- 10
      b <- parameter()
      dim(b) <- c(3, 10)
      v[] <- interpolate(a, b, "constant")
      update(x) <- sum(v)
      initial(x) <- 0
      dim(v) <- 3
    }),
    "Found 1 compatibility issue")
  expect_match(conditionMessage(w),
               "Drop arrays from lhs of assignments from 'interpolate()'",
               fixed = TRUE)
})
