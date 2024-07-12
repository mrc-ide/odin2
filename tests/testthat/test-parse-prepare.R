test_that("can detect expression as an expression", {
  expr <- quote({a <- 1})
  quo <- rlang::new_quosure(expr)
  expect_equal(
    parse_prepare_detect(quo, NULL, NULL),
    list(type = "expression",
         value = expr))
  expect_equal(
    parse_prepare_detect(quo, "expression", NULL),
    parse_prepare_detect(quo, NULL, NULL))
})


test_that("can detect a path as a filename", {
  path <- withr::local_tempfile()
  quo <- rlang::new_quosure(path)
  writeLines("a <- 1", path)
  expect_equal(parse_prepare_detect(quo, NULL, NULL),
               list(type = "file", value = path))
  expect_equal(parse_prepare_detect(quo, "file", NULL),
               parse_prepare_detect(quo, NULL, NULL))
})


test_that("can detect code as a string", {
  code <- "a <- 1\nb <- 2"
  expect_equal(
    parse_prepare_detect(rlang::new_quosure(code), NULL, NULL),
    list(type = "text", value = code))
})


test_that("can detect code as a character vector", {
  code <- c("a <- 1", "b <- 2")
  expect_equal(
    parse_prepare_detect(rlang::new_quosure(code), NULL, NULL),
    list(type = "text", value = code))
})


test_that("throw error on unclassifiable input", {
  expect_error(
    parse_prepare_detect(rlang::new_quosure(NULL), NULL, NULL),
    "Invalid input for odin; expected a string, character vector or")
  expect_error(
    parse_prepare_detect(rlang::new_quosure(TRUE), NULL, NULL),
    "Invalid input for odin; expected a string, character vector or")
})


test_that("sensible error if requiring expression but given other type", {
  expect_error(
    parse_prepare_detect(rlang::new_quosure("a <- 1"), "expression", NULL),
    "Invalid input for odin; given string but expected expression")
  expect_error(
    parse_prepare_detect(rlang::new_quosure(NULL), "expression", NULL),
    "Invalid input for odin; expected an expression")
})


test_that("sensible error if requiring a file but given other type", {
  expect_error(
    parse_prepare_detect(rlang::new_quosure(quote({a <- 1})), "file", NULL),
    "Invalid input for odin; given expression but expected file")
  expect_error(
    parse_prepare_detect(rlang::new_quosure(NULL), "file", NULL),
    "Invalid input for odin; expected a string")
})


test_that("sensible error if requiring a file but the file does not exist", {
  quo <- rlang::new_quosure("somepath.R")
  expect_error(
    parse_prepare_detect(quo, "file", NULL),
    "File 'somepath.R' does not exist")
})


test_that("sensible error if file argument is invalid", {
  expect_error(
    parse_prepare_detect(rlang::new_quosure(character()), "file", NULL),
    "Invalid input for odin; expected a scalar for 'expr'")
  expect_error(
    parse_prepare_detect(rlang::new_quosure(letters), "file", NULL),
    "Invalid input for odin; expected a scalar for 'expr'")
})


test_that("sensible error if given a non-existant file-like string", {
  expect_error(
    parse_prepare_detect(rlang::new_quosure("foo.R"), NULL, NULL),
    "'foo.R' looks like a filename but does not exist")
})


test_that("sensible error if requiring text but given other type", {
  expect_error(
    parse_prepare_detect(rlang::new_quosure(quote({a <- 1})), "text", NULL),
    "Invalid input for odin; given expression but expected text")
  expect_error(
    parse_prepare_detect(rlang::new_quosure(NULL), "text", NULL),
    "Invalid input for odin; expected a string or character vector")
})


test_that("redirect through symbols", {
  a <- "a <- 1\nb <- 2"
  expect_equal(
    parse_prepare_detect(rlang::new_quosure(quote(a)), "text", NULL),
    list(type = "text",
         value = a))
  expect_error(
    parse_prepare_detect(rlang::new_quosure(quote(asdfa)), "text", NULL),
    "Could not find expression 'asdfa'")
})


test_that("can load code from an expression", {
  quo <- rlang::new_quosure(quote({
    a <- 1
    b <- 2
  }))
  expect_equal(
    parse_prepare(quo, NULL, NULL),
    list(type = "expression",
         filename = NULL,
         exprs = list(list(value = quote(a <- 1)),
                      list(value = quote(b <- 2)))))
})


test_that("fail if an extra level of quoting is present", {
  quo <- rlang::new_quosure(quote(quote({a <- 1})))
  expect_error(
    parse_prepare(quo, NULL, NULL),
    "You have an extra layer of 'quote()' around 'expr'",
    fixed = TRUE)
})


test_that("require that expr is multiline", {
  quo <- rlang::new_quosure(quote(a <- 1))
  expect_error(
    parse_prepare(quo, NULL, NULL),
    "Expected 'expr' to be a multiline expression within curly braces",
    fixed = TRUE)
})


test_that("can read expressions from file", {
  path <- withr::local_tempfile()
  writeLines(c("a <- 1", "b <- 2"), path)
  expect_equal(
    parse_prepare(rlang::new_quosure(path), "file", NULL),
    list(type = "file",
         filename = path,
         exprs = list(list(value = quote(a <- 1),
                           start = 1,
                           end = 1,
                           str = "a <- 1"),
                      list(value = quote(b <- 2),
                           start = 2,
                           end = 2,
                           str = "b <- 2"))))
})


test_that("can read expressions from file", {
  code <- c("a <- 1", "b <- 2")
  expect_equal(
    parse_prepare(rlang::new_quosure(code), "text", NULL),
    list(type = "text",
         filename = NULL,
         exprs = list(list(value = quote(a <- 1),
                           start = 1,
                           end = 1,
                           str = "a <- 1"),
                      list(value = quote(b <- 2),
                           start = 2,
                           end = 2,
                           str = "b <- 2"))))
})
