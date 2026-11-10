test_that("can generate basic literal values", {
  dat <- generate_dust_dat(NULL, NULL, NULL, NULL)
  options <- list()

  expect_equal(generate_js_sexp(TRUE, dat, options), "true")
  expect_equal(generate_js_sexp(FALSE, dat, options), "false")

  expect_equal(generate_js_sexp(1, dat, options), "1")
  expect_equal(generate_js_sexp(10, dat, options), "10")

  expect_equal(generate_js_sexp(1.1, dat, options),
               sprintf("%s", deparse(1.1, control = "digits17")))
})


test_that("can generate min/max expressions", {
  dat <- generate_dust_dat(c(a = "shared", b = "stack"), NULL, NULL, NULL)
  options <- list()
  expect_equal(generate_js_sexp(quote(min(a, b)), dat, options),
               "Math.min(params.a, b)")
  expect_equal(generate_js_sexp(quote(max(a, 1)), dat, options),
               "Math.max(params.a, 1)")
  expect_equal(generate_js_sexp(quote(min(2, 1)), dat, options),
               "Math.min(2, 1)")
})


test_that("can generate min/max expressions for index", {
  dat <- generate_dust_dat(c(a = "shared", b = "stack", x = "internal"),
                           NULL, NULL, NULL)
  options <- list()
  expect_equal(generate_js_sexp(quote(x[min(a, b)]), dat, options),
               "internal.x[Math.round(Math.min(params.a, b)) - 1]")
  expect_equal(generate_js_sexp(quote(x[max(a, 1)]), dat, options),
               "internal.x[Math.round(Math.max(params.a, 1)) - 1]")
  expect_equal(generate_js_sexp(quote(x[min(2, 1)]), dat, options),
               "internal.x[Math.round(Math.min(2, 1)) - 1]")
})


test_that("can generate integer maths functions", {
  dat <- generate_dust_dat(c(a = "shared", b = "stack"), NULL, NULL, NULL)
  options <- list()
  expect_equal(generate_js_sexp(quote(a %% b), dat, options),
               "params.a % b")
  expect_equal(generate_js_sexp(quote(a %/% b), dat, options),
               "Math.floor(params.a / b)")
})


test_that("can generate robust code for division", {
  dat <- generate_dust_dat(c(a = "shared", b = "stack"), NULL, NULL, NULL)
  options <- list()
  expect_equal(generate_js_sexp(quote(a / b), dat, options),
               "params.a / b")
  expect_equal(generate_js_sexp(quote(1 / 2), dat, options),
               "1 / 2")
})


test_that("can cast types", {
  dat <- generate_dust_dat(c(a = "shared"), NULL, NULL, NULL)
  options <- list()
  expect_equal(generate_js_sexp(quote(as.integer(1)), dat, options),
               "Math.round(1)")
  expect_equal(generate_js_sexp(quote(as.integer(a)), dat, options),
               "Math.round(params.a)")
  expect_equal(generate_js_sexp(quote(as.numeric(1)), dat, options),
               "Number(1)")
  expect_equal(generate_js_sexp(quote(as.numeric(a)), dat, options),
               "Number(params.a)")
})


test_that("time and dt are always available", {
  dat <- generate_dust_dat(NULL, NULL, NULL, NULL)
  options <- list()
  expect_equal(generate_js_sexp("time", dat, options), "time")
  expect_equal(generate_js_sexp("dt", dat, options), "dt")
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
    generate_js_sexp(quote(a + b), dat, options),
    "a + b")
  expect_equal(
    generate_js_sexp(quote(a + b * c), dat, options),
    "a + b * params.c")
  expect_equal(
    generate_js_sexp(quote(a + b * c - d), dat, options),
    "a + b * params.c - internal.d")
  expect_equal(
    generate_js_sexp(quote(-4), dat, options),
    "-4")
  expect_equal(
    generate_js_sexp(quote(+4), dat, options),
    "4")
  expect_equal(
    generate_js_sexp(quote(3 * (a + b)), dat, options),
    "3 * (a + b)")
})


test_that("can use functions from the library", {
  dat <- generate_dust_dat(NULL, NULL, NULL, NULL)
  options <- list()
  expect_equal(
    generate_js_sexp(quote(3 * exp(1 + 2)), dat, options),
    "3 * Math.exp(1 + 2)")
  expect_equal(
    generate_js_sexp(quote(4^8), dat, options),
    "Math.pow(4, 8)")
  expect_equal(
    generate_js_sexp(quote(ceiling(4)), dat, options),
    "Math.ceil(4)")
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
    generate_js_sexp(quote(a + as.integer(b)), dat, NULL),
    "a + Math.round(b)")
  expect_equal(
    generate_js_sexp(quote(c + as.numeric(d)), dat, NULL),
    "params.c + Number(internal.d)")
  expect_equal(
    generate_js_sexp(quote(as.logical(1)), dat, NULL),
    "Boolean(1)")
})


test_that("unsupported functions are bugs", {
  dat <- generate_dust_dat(NULL, NULL, NULL, NULL)
  expect_error(
    generate_js_sexp(quote(fn(1)), dat, list()),
    "Unhandled function 'fn'",
    class = "odin_bug")
})


test_that("unsupported types are bugs", {
  dat <- generate_dust_dat(NULL, NULL, NULL, NULL)
  expect_error(
    generate_js_sexp(NULL, dat, list()),
    "Unhandled data type while generating expression",
    class = "odin_bug")
})


test_that("can generate special maths bits", {
  dat <- generate_dust_dat(NULL, NULL, NULL, NULL)
  expect_equal(generate_js_sexp(quote(2 * pi), dat, list()), "2 * Math.PI")
  expect_equal(
    generate_js_sexp(quote(Inf), dat, list()),
    "Infinity")
  expect_equal(
    generate_js_sexp(quote(-Inf), dat, list()),
    "-Infinity")
})
