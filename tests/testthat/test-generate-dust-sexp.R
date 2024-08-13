test_that("can generate basic literal values", {
  dat <- generate_dust_dat(NULL)
  options <- list()

  expect_equal(generate_dust_sexp(TRUE, dat, options), "true")
  expect_equal(generate_dust_sexp(FALSE, dat, options), "false")

  expect_equal(generate_dust_sexp(1, dat, options), "1")
  expect_equal(generate_dust_sexp(10, dat, options), "10")

  expect_equal(generate_dust_sexp(1.1, dat, options),
               sprintf("static_cast<real_type>(%s)",
                       deparse(1.1, control = "digits17")))
})


test_that("can generate references to bits of data", {
  dat <- generate_dust_dat(c(a = "state",
                             b = "stack",
                             c = "shared",
                             d = "internal",
                             e = "data"))
  options <- list()
  expect_equal(generate_dust_sexp("a", dat, options), "a")
  expect_equal(generate_dust_sexp("b", dat, options), "b")
  expect_equal(generate_dust_sexp("c", dat, options), "shared.c")
  expect_equal(generate_dust_sexp("d", dat, options), "internal.d")
  expect_equal(generate_dust_sexp("e", dat, options), "data.e")

  options <- list(shared_exists = FALSE)

  expect_equal(generate_dust_sexp("a", dat, options), "a")
  expect_equal(generate_dust_sexp("b", dat, options), "b")
  expect_equal(generate_dust_sexp("c", dat, options), "c")
  expect_equal(generate_dust_sexp("d", dat, options), "internal.d")
  expect_equal(generate_dust_sexp("e", dat, options), "data.e")
})


test_that("time and dt are always available", {
  dat <- generate_dust_dat(c())
  options <- list()
  expect_equal(generate_dust_sexp("time", dat, options), "time")
  expect_equal(generate_dust_sexp("dt", dat, options), "dt")
})


test_that("can generate simple expressions involving arithmetic", {
  dat <- generate_dust_dat(c(a = "state",
                             b = "stack",
                             c = "shared",
                             d = "internal",
                             e = "data"))
  options <- list()

  expect_equal(
    generate_dust_sexp(quote(a + b), dat, options),
    "a + b")
  expect_equal(
    generate_dust_sexp(quote(a + b * c), dat, options),
    "a + b * shared.c")
  expect_equal(
    generate_dust_sexp(quote(a + b * c - d), dat, options),
    "a + b * shared.c - internal.d")
  expect_equal(
    generate_dust_sexp(quote(-4), dat, options),
    "-4")
  expect_equal(
    generate_dust_sexp(quote(+4), dat, options),
    "4")
  expect_equal(
    generate_dust_sexp(quote(3 * (a + b)), dat, options),
    "3 * (a + b)")
})


test_that("can use functions from the library", {
  dat <- generate_dust_dat(NULL)
  options <- list()
  expect_equal(
    generate_dust_sexp(quote(3 * exp(1 + 2)), dat, options),
    "3 * mcstate::math::exp(1 + 2)")
  expect_equal(
    generate_dust_sexp(quote(4^8), dat, options),
    "mcstate::math::pow(4, 8)")
  expect_equal(
    generate_dust_sexp(quote(ceiling(4)), dat, options),
    "mcstate::math::ceil(4)")
})


test_that("unsupported functions are bugs", {
  dat <- generate_dust_dat(NULL)
  expect_error(
    generate_dust_sexp(quote(fn(1)), dat, list()),
    "Unhandled function 'fn'",
    class = "odin_bug")
})


test_that("unsupported types are bugs", {
  dat <- generate_dust_dat(NULL)
  expect_error(
    generate_dust_sexp(NULL, dat, list()),
    "Unhandled data type while generating expression",
    class = "odin_bug")
})
