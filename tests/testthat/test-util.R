test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
})


test_that("match_value", {
  expect_error(match_value("foo", letters), "must be one of")
  expect_silent(match_value("a", letters))
})


test_that("can match a simple call", {
  fn <- function(foo, bar = 1) NULL
  expect_equal(
    match_call(quote(f(a)), fn),
    list(success = TRUE, value = quote(f(foo = a, bar = 1))))
  expect_equal(
    match_call(quote(f(a, b)), fn),
    list(success = TRUE, value = quote(f(foo = a, bar = b))))
  expect_equal(
    match_call(quote(f(bar = 2, foo = 1)), fn),
    list(success = TRUE, value = quote(f(foo = 1, bar = 2))))

  res <- match_call(quote(f(baz = 1)), fn)
  expect_false(res$success)

  ## Partial matching still enabled
  expect_equal(
    suppressWarnings(match_call(quote(f(fo = 2)), fn)),
    list(success = TRUE, value = quote(f(foo = 2, bar = 1))))
})


test_that("set_names copes with common pathologies", {
  expect_equal(set_names(character(), "x"),
               structure(character(), names = character()))
  expect_equal(set_names("a", "x"),
               c("x" = "a"))
  expect_equal(set_names(c("a", "b"), "x"),
               c("x" = "a", x = "b"))
  expect_equal(set_names(c("a", "b"), c("x", "y")),
               c("x" = "a", y = "b"))
  expect_null(set_names(NULL, "x"))
})


test_that("detect unary minus", {
  expect_false(uses_unary_minus(quote(a - b)))
  expect_true(uses_unary_minus(quote(-a - b)))
})


test_that("can generate reasonable expressions for addition", {
  expect_identical(
    expr_plus(quote(a), quote(b)), quote(a + b))
  expect_identical(
    expr_plus(quote(a), 1), quote(a + 1))
  expect_identical(
    expr_plus(1, quote(b)), quote(1 + b))
  expect_identical(
    expr_plus(1, 2), 3)
})


test_that("can generate reasonable expressions for subtraction", {
  expect_identical(
    expr_minus(quote(a), quote(b)), quote(a - b))
  expect_identical(
    expr_minus(quote(a), 1), quote(a - 1))
  expect_identical(
    expr_minus(1, quote(b)), quote(1 - b))
  expect_identical(
    expr_minus(3, 2), 1)
})


test_that("writelines_if_changed doesn't replace file", {
  workdir <- withr::local_tempdir()
  path <- "myfile"
  path_full <- file.path(workdir, path)
  text1 <- c("a", "b", "c")
  text2 <- c("a", "b", "c", "d")
  str1 <- paste(text1, collapse = "\n")
  str2 <- structure(str1, class = "glue")
  expect_silent(writelines_if_changed(text1, workdir, path, quiet = TRUE))
  expect_true(same_content(path_full, text1))
  expect_true(same_content(path_full, str1))
  expect_true(same_content(path_full, str2))
  expect_silent(writelines_if_changed(str1, workdir, path, quiet = TRUE))
  expect_silent(writelines_if_changed(str2, workdir, path, quiet = TRUE))
  expect_true(file.exists(path_full))
  expect_equal(readLines(path_full), text1)
  t <- file.mtime(path_full)
  expect_silent(writelines_if_changed(text1, workdir, path, quiet = TRUE))
  expect_identical(file.mtime(path_full), t)
  expect_silent(writelines_if_changed(text2, workdir, path, quiet = TRUE))
  expect_equal(readLines(path_full), text2)
  ## I don't trust times and sub-second accuracy not guaranted; see
  ## ?file.mtime
  skip_on_cran()
  skip_on_os("windows")
  expect_gt(file.mtime(path_full), t)
})


test_that("tell user about changes to files", {
  workdir <- withr::local_tempdir()

  text1 <- c("a", "b", "c")
  text2 <- c("a", "b", "c", "d")
  str1 <- paste(text1, collapse = "\n")
  str2 <- paste(text2, collapse = "\n")
  expect_message(
    writelines_if_changed(text1, workdir, "myfile", FALSE),
    "Wrote 'myfile'")
  expect_message(
    writelines_if_changed(text1, workdir, "myfile", FALSE),
    "'myfile' is up to date")
  expect_message(
    writelines_if_changed(text2, workdir, "myfile", FALSE),
    "Wrote 'myfile'")
})
