test_that("can generate code", {
  res <- odin_show({
    initial(x) <- 1
    update(x) <- x + 1
  })
  expect_s3_class(res, "odin_code")
  expect_type(res, "character")
})


test_that("can print odin_code", {
  code <- structure(c("a", "b", "c"), class = "odin_code")
  res <- evaluate_promise(withVisible(print(code)))
  expect_equal(res$result, list(value = code, visible = FALSE))
  expect_match(res$messages, "odin code:", all = FALSE)
  expect_equal(res$output, paste(code, collapse = "\n"))
})
