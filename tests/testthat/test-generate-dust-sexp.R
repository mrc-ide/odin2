test_that("can generate basic literal values", {
  dat <- generate_dust_dat(NULL, NULL, NULL, NULL)
  options <- list()

  expect_equal(generate_dust_sexp(TRUE, dat, options), "true")
  expect_equal(generate_dust_sexp(FALSE, dat, options), "false")

  expect_equal(generate_dust_sexp(1, dat, options), "1")
  expect_equal(generate_dust_sexp(10, dat, options), "10")

  expect_equal(generate_dust_sexp(1.1, dat, options),
               sprintf("static_cast<real_type>(%s)",
                       deparse(1.1, control = "digits17")))
})


test_that("can generate min/max expressions", {
  dat <- generate_dust_dat(c(a = "shared", b = "stack"), NULL, NULL, NULL)
  options <- list()
  expect_equal(generate_dust_sexp(quote(min(a, b)), dat, options),
               "monty::math::min(shared.a, b)")
  expect_equal(generate_dust_sexp(quote(max(a, 1)), dat, options),
               "monty::math::max(shared.a, static_cast<real_type>(1))")
  expect_equal(generate_dust_sexp(quote(min(2, 1)), dat, options),
               "monty::math::min(2, 1)")
})


test_that("can generate integer maths functions", {
  dat <- generate_dust_dat(c(a = "shared", b = "stack"), NULL, NULL, NULL)
  options <- list()
  expect_equal(generate_dust_sexp(quote(a %% b), dat, options),
               "std::fmod(shared.a, b)")
  expect_equal(generate_dust_sexp(quote(a %/% b), dat, options),
               "monty::math::fintdiv(shared.a, b)")
})


test_that("can cast types", {
  dat <- generate_dust_dat(c(a = "shared"), NULL, NULL, NULL)
  options <- list()
  expect_equal(generate_dust_sexp(quote(as.integer(1)), dat, options),
               "static_cast<int>(1)")
  expect_equal(generate_dust_sexp(quote(as.integer(a)), dat, options),
               "static_cast<int>(shared.a)")
  expect_equal(generate_dust_sexp(quote(as.numeric(1)), dat, options),
               "static_cast<real_type>(1)")
  expect_equal(generate_dust_sexp(quote(as.numeric(a)), dat, options),
               "static_cast<real_type>(shared.a)")
})


test_that("time and dt are always available", {
  dat <- generate_dust_dat(NULL, NULL, NULL, NULL)
  options <- list()
  expect_equal(generate_dust_sexp("time", dat, options), "time")
  expect_equal(generate_dust_sexp("dt", dat, options), "dt")
})


test_that("can generate simple expressions involving arithmetic", {
  dat <- generate_dust_dat(
    c(a = "state",
      b = "stack",
      c = "shared",
      d = "internal",
      e = "data"),
    packing = list(state = parse_packing("a", NULL, NULL, "state")),
    type = NULL,
    arrays = NULL)
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
  dat <- generate_dust_dat(NULL, NULL, NULL, NULL)
  options <- list()
  expect_equal(
    generate_dust_sexp(quote(3 * exp(1 + 2)), dat, options),
    "3 * monty::math::exp(1 + 2)")
  expect_equal(
    generate_dust_sexp(quote(4^8), dat, options),
    "monty::math::pow(4, 8)")
  expect_equal(
    generate_dust_sexp(quote(ceiling(4)), dat, options),
    "monty::math::ceil(4)")
})


test_that("can coerce to different types", {
  dat <- generate_dust_dat(
    c(a = "state",
      b = "stack",
      c = "shared",
      d = "internal",
      e = "data"),
    packing = list(state = parse_packing("a", NULL, NULL, "state")),
    type = NULL,
    arrays = NULL)
  options <- list()
  expect_equal(
    generate_dust_sexp(quote(a + as.integer(b)), dat, NULL),
    "a + static_cast<int>(b)")
  expect_equal(
    generate_dust_sexp(quote(c + as.numeric(d)), dat, NULL),
    "shared.c + static_cast<real_type>(internal.d)")
  expect_equal(
    generate_dust_sexp(quote(as.logical(1)), dat, NULL),
    "static_cast<bool>(1)")
})


test_that("can generate min/max", {
  dat <- generate_dust_dat(
    c(a = "state",
      b = "stack",
      c = "shared",
      d = "internal",
      e = "data"),
    packing = list(state = parse_packing("a", NULL, NULL, "state")),
    type = NULL,
    arrays = NULL)
  options <- list()
  expect_equal(
    generate_dust_sexp(quote(3 * exp(1 + 2)), dat, options),
    "3 * monty::math::exp(1 + 2)")
  expect_equal(
    generate_dust_sexp(quote(4^8), dat, options),
    "monty::math::pow(4, 8)")
  expect_equal(
    generate_dust_sexp(quote(ceiling(4)), dat, options),
    "monty::math::ceil(4)")
})


test_that("unsupported functions are bugs", {
  dat <- generate_dust_dat(NULL, NULL, NULL, NULL)
  expect_error(
    generate_dust_sexp(quote(fn(1)), dat, list()),
    "Unhandled function 'fn'",
    class = "odin_bug")
})


test_that("unsupported types are bugs", {
  dat <- generate_dust_dat(NULL, NULL, NULL, NULL)
  expect_error(
    generate_dust_sexp(NULL, dat, list()),
    "Unhandled data type while generating expression",
    class = "odin_bug")
})
