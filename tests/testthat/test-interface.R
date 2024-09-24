test_that("can parse files where we pass an expression for filename", {
  tmp <- withr::local_tempfile()
  writeLines(c("initial(x) <- 0", "update(x) <- x + 1"), tmp)
  expect_equal(odin_parse(tmp)$time, "discrete")

  dir <- dirname(tmp)
  base <- basename(tmp)
  expect_equal(odin_parse(file.path(dir, base))$time, "discrete")
})
