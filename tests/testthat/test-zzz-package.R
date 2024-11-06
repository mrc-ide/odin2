test_that("can generate trivial package, compile the result, and run system", {
  path <- withr::local_tempdir()
  test_pkg_setup(path, "mypkg")
  writeLines(c("initial(x) <- 0",
               "update(x) <- x + 1"),
             file.path(path, "inst/odin/a.R"))
  res <- odin_package(path, quiet = TRUE)
  pkg <- pkgload::load_all(res, quiet = TRUE)

  expect_s3_class(pkg$env$a(), "dust_system_generator")
  sys <- dust2::dust_system_create(pkg$env$a(), list(), n_particles = 1)
  y <- dust2::dust_system_simulate(sys, 0:10)
  expect_equal(y, rbind(0:10))
})


test_that("can run example package", {
  path <- withr::local_tempdir()
  example_package(path)
  res <- odin_package(path, quiet = TRUE)
  pkg <- pkgload::load_all(res, quiet = TRUE)
  expect_s3_class(pkg$env$sir, "dust_system_generator")
})
