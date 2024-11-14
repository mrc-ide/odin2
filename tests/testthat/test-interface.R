test_that("can parse files where we pass an expression for filename", {
  tmp <- withr::local_tempfile()
  writeLines(c("initial(x) <- 0", "update(x) <- x + 1"), tmp)
  expect_equal(odin_parse(tmp)$time, "discrete")

  dir <- dirname(tmp)
  base <- basename(tmp)
  expect_equal(odin_parse(file.path(dir, base))$time, "discrete")
})


test_that("odin calls dust", {
  skip_if_not_installed("mockery")
  mock_dust_compile <- mockery::mock()
  mockery::stub(odin, "dust2::dust_compile", mock_dust_compile)
  odin({
    initial(a) <- 0
    update(a) <- a + 1
  })

  str <- odin_show({
    initial(a) <- 0
    update(a) <- a + 1
  })

  mockery::expect_called(mock_dust_compile, 1)
  args <- mockery::mock_args(mock_dust_compile)[[1]]
  expect_match(args[[1]], "\\.cpp$")
  expect_equal(
    args[-1],
    list(quiet = NULL, workdir = NULL, debug = NULL, skip_cache = FALSE))
})
