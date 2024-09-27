test_that("can validate simple model and return metadata", {
  res <- odin_validate({
    initial(x) <- 1
    update(x) <- Normal(x, a)
    a <- parameter()
    d <- data()
    d ~ Normal(x, 1)
  })
  expect_true(res$success)
  expect_null(res$error)
  expect_null(res$compatibility)
  expect_equal(
    res$result,
    list(time = "discrete",
         variables = data_frame(name = "x"),
         parameters = data_frame(name = "a"),
         data = data_frame(name = "d")))
})


test_that("can get error from odin code", {
  res <- odin_validate({
    initial(x) <- 1
    update(x) <- a
  })
  expect_false(res$success)
  expect_null(res$result)
  expect_null(res$compatibility)
  expect_s3_class(res$error, "odin_parse_error")
  expect_match(res$error$message, "Unknown variable used in odin code")
  ## We'll tidy this up later.
  expect_equal(res$error$src[[1]]$index, 2)
})


test_that("can get migration warnings", {
  res <- odin_validate({
    initial(x) <- 1
    update(x) <- a
    a <- user()
  })
  expect_true(res$success)
  expect_s3_class(res$compatibility, "data.frame")
  expect_equal(nrow(res$compatibility), 1)
  expect_equal(res$compatibility$index, 3)
  expect_equal(res$compatibility$type, "user")
})


test_that("can get migration warnings with original source", {
  tmp <- withr::local_tempfile()
  writeLines(
    c("initial(x) <- 1",
      "update(x) <- a",
      "a <-",
      "  user()"),
    tmp)

  res <- odin_validate(tmp)
  expect_equal(res$compatibility$src_value,
               "a <-\n  user()")
})
