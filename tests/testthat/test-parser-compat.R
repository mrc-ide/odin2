test_that("can translate user()", {
  d <- odin_parse({
    a <- user(1)
    initial(x) <- 0
    update(x) <- x + a
  })

  expect_equal(d$equations$a$src$value,
               quote(a <- parameter(1)))
  expect_equal(d$equations$a$compat,
               list(type = "user", original = quote(a <- user(1))))
})


test_that("can translate simple user calls", {
  expect_equal(
    parse_compat_fix_user(list(value = quote(a <- user()))),
    list(value = quote(a <- parameter()),
         compat = list(type = "user", original = quote(a <- user()))))
  expect_equal(
    parse_compat_fix_user(list(value = quote(a <- user(1)))),
    list(value = quote(a <- parameter(1)),
         compat = list(type = "user", original = quote(a <- user(1)))))
  expect_error(
    parse_compat_fix_user(list(value = quote(a <- user(integer = TRUE)))),
    "Can't translate 'user()' calls that use the 'integer' argument",
    fixed = TRUE,
    class = "odin_parse_error")
  expect_error(
    parse_compat_fix_user(list(value = quote(a <- user(min = 0)))),
    "Can't translate 'user()' calls that use the 'min' argument",
    fixed = TRUE,
    class = "odin_parse_error")
  expect_error(
    parse_compat_fix_user(list(value = quote(a <- user(max = 1)))),
    "Can't translate 'user()' calls that use the 'max' argument",
    fixed = TRUE,
    class = "odin_parse_error")
})


test_that("handle errors that occur in translated code", {
  err <- expect_error(
    odin_parse({
      a <- user(sqrt(b))
      b <- 1
      initial(x) <- 0
      update(x) <- x + a
    }),
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
