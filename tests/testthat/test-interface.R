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
  mock_dust_generator <- mockery::mock()
  mockery::stub(odin, "odin_generator_cpp", mock_dust_generator)
  odin({
    initial(a) <- 0
    update(a) <- a + 1
  })

  str <- odin_show({
    initial(a) <- 0
    update(a) <- a + 1
  })

  mockery::expect_called(mock_dust_generator, 1)
  args <- mockery::mock_args(mock_dust_generator)[[1]]
  expect_equal(
    args[-1],
    list(quiet = NULL, workdir = NULL, debug = NULL, skip_cache = FALSE))
})


test_that("can request js generation", {
  expect_error(
    odin({
      initial(a) <- 0
      deriv(a) <- 1
    }, target = "js"),
    "JavaScript is not yet supported")
})


test_that("can select targets", {
  withr::with_options(list(odin2.target = "js"), {
    expect_equal(odin_select_target(NULL), "js")
    expect_equal(odin_select_target("cpp"), "cpp")
    expect_equal(odin_select_target("js"), "js")
  })

  withr::with_options(list(odin2.target = NULL), {
    expect_equal(odin_select_target(NULL), "cpp")
    expect_equal(odin_select_target("cpp"), "cpp")
    expect_equal(odin_select_target("js"), "js")
  })

  expect_error(odin_select_target("other"),
               "'target' must be one of 'cpp', 'js'")
})
