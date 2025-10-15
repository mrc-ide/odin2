test_that("can request js generation", {
  expect_error(
    odin({
      initial(a) <- 0
      deriv(a) <- 1
    }, target = "js"),
    "JavaScript is not yet supported")
})


test_that("can select targets", {
  withr::with_option(odin2.target = "js", {
    expect_equal(odin_select_target(NULL), "js")
    expect_equal(odin_select_target("cpp"), "js")
    expect_equal(odin_select_target("js"), "js")
  })

  withr::with_option(odin2.target = NULL, {
    expect_equal(odin_select_target(NULL), "cpp")
    expect_equal(odin_select_target("cpp"), "js")
    expect_equal(odin_select_target("js"), "js")
  })

  expect_error(odin_select_target("other"),
               "'target' must be one of 'cpp', 'js'")
})
