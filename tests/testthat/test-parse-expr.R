test_that("can parse simple assignments", {
  res <- parse_expr(quote(a <- 1), NULL, NULL)
  expect_equal(res$lhs$name, "a")
  expect_equal(res$rhs$type, "expression")
  expect_equal(res$rhs$expr, 1)
  expect_equal(res$rhs$depends,
               list(functions = character(), variables = character()))
})


test_that("can parse simple expressions involving functions/variables", {
  res <- parse_expr(quote(a <- b + c / b), NULL, NULL)
  expect_equal(res$rhs$expr, quote(b + c / b))
  expect_equal(res$rhs$depends,
               list(functions = c("+", "/"), variables = c("b", "c")))
})


test_that("require that assignment lhs is reasonable", {
  expect_error(
    parse_expr(quote(1 <- 1), NULL, NULL),
    "Invalid target '1' on the lhs of assignment")
  expect_error(
    parse_expr(quote(update(1) <- 1), NULL, NULL),
    "Invalid target '1' within 'update()' on the lhs of assignment",
    fixed = TRUE)
  expect_error(
    parse_expr(quote(f(1) <- 1), NULL, NULL),
    "Invalid special function 'f()' on the lhs of assignment",
    fixed = TRUE)
  expect_error(
    parse_expr(quote(compare(x) <- Normal(0, 1)), NULL, NULL),
    "Invalid special function 'compare()' on the lhs of assignment",
    fixed = TRUE)
  expect_error(
    parse_expr(quote(update(f(x)) <- 1), NULL, NULL),
    "Invalid target 'f(x)' within 'update()' on the lhs of assignment",
    fixed = TRUE)
})


## Special calls are initial/deriv/update/dim/output/config/compare
test_that("allow calls on lhs", {
  res <- parse_expr(quote(initial(x) <- 1), NULL, NULL)
  expect_equal(res$lhs$name, "x")
  expect_equal(res$special, "initial")
  expect_equal(res$rhs$expr, 1)
  expect_equal(parse_expr(quote(deriv(x) <- 1), NULL, NULL)$special, "deriv")
  expect_equal(parse_expr(quote(update(x) <- 1), NULL, NULL)$special, "update")
})


test_that("require that special calls are (currently) simple", {
  expect_error(
    parse_expr(quote(update(x, TRUE) <- 1), NULL, NULL),
    "Invalid call to special function 'update'")
  expect_error(
    parse_expr(quote(initial() <- 1), NULL, NULL),
    "Invalid call to special function 'initial'")
  err <- expect_error(
    parse_expr(quote(initial(x = 1) <- 1), NULL, NULL),
    "Invalid call to special function 'initial'")
})


test_that("can parse parameter definitions", {
  res <- parse_expr(quote(a <- parameter()), NULL, NULL)
  expect_equal(res$rhs$type, "parameter")
  expect_null(res$rhs$args$default)
  expect_equal(res$rhs$args$constant, NA)
  expect_false(res$rhs$args$differentiate)
})


test_that("can parse parameter definitions with defaults", {
  res <- parse_expr(quote(a <- parameter(10)), NULL, NULL)
  expect_equal(res$rhs$type, "parameter")
  expect_equal(res$rhs$args$default, 10)
  expect_equal(res$rhs$args$constant, NA)
  expect_false(res$rhs$args$differentiate)
})


test_that("can parse parameter definitions with expression defaults", {
  res <- parse_expr(quote(a <- parameter(-4 / 3)), NULL, NULL)
  expect_equal(res$rhs$type, "parameter")
  expect_equal(res$rhs$args$default, quote(-4 / 3))
  expect_equal(res$rhs$args$constant, NA)
  expect_false(res$rhs$args$differentiate)
})


test_that("parameter defaults must be simple", {
  expect_error(
    parse_expr(quote(a <- parameter(a)), NULL, NULL),
    "Invalid default argument to 'parameter()': a",
    fixed = TRUE)
})


test_that("validate differentiate argument", {
  res <- parse_expr(quote(a <- parameter(differentiate = TRUE)), NULL, NULL)
  expect_true(res$rhs$args$differentiate)
  res <- parse_expr(quote(a <- parameter()), NULL, NULL)
  expect_false(res$rhs$args$differentiate)

  expect_error(
    parse_expr(quote(a <- parameter(differentiate = x)), NULL, NULL),
    "'differentiate' must be a scalar logical, but was 'x'")
  expect_error(
    parse_expr(quote(a <- parameter(differentiate = NA)), NULL, NULL),
    "'differentiate' must be a scalar logical, but was 'NA'")
  expect_error(
    parse_expr(quote(a <- parameter(differentiate = NULL)), NULL, NULL),
    "'differentiate' must be a scalar logical, but was 'NULL'")
})


test_that("validate constant argument", {
  res <- parse_expr(quote(a <- parameter(constant = TRUE)), NULL, NULL)
  expect_true(res$rhs$args$constant)
  res <- parse_expr(quote(a <- parameter(constant = FALSE)), NULL, NULL)
  expect_false(res$rhs$args$constant)
  res <- parse_expr(quote(a <- parameter()), NULL, NULL)
  expect_equal(res$rhs$args$constant, NA)

  expect_error(
    parse_expr(quote(a <- parameter(constant = x)), NULL, NULL),
    "'constant' must be a scalar logical if given, but was 'x'")
  expect_error(
    parse_expr(quote(a <- parameter(constant = NA)), NULL, NULL),
    "'constant' must be a scalar logical if given, but was 'NA'")
})


test_that("can't use both constant and differentiate", {
  expect_error(
    parse_expr(quote(a <- parameter(differentiate = TRUE, constant = TRUE)),
               NULL, NULL),
    "Differentiable parameters must not be constant")
})


test_that("differentiable parameters are not constant", {
  res <- parse_expr(quote(a <- parameter(differentiate = TRUE)),
                    NULL, NULL)
  expect_mapequal(res$rhs$args,
                  list(default = NULL,
                       constant = FALSE,
                       differentiate = TRUE,
                       type = "real_type",
                       rank = NULL,
                       min = NULL,
                       max = NULL))
})

test_that("can change parameter type", {
  res <- parse_expr(quote(a <- parameter()), NULL, NULL)
  expect_equal(res$rhs$args$type, NA_character_)
  res <- parse_expr(quote(a <- parameter(type = "real")), NULL, NULL)
  expect_equal(res$rhs$args$type, "real_type")
  res <- parse_expr(quote(a <- parameter(type = "integer")), NULL, NULL)
  expect_equal(res$rhs$args$type, "int")
  res <- parse_expr(quote(a <- parameter(type = "logical")), NULL, NULL)
  expect_equal(res$rhs$args$type, "bool")
})


test_that("validate type arg to parameter", {
  expect_error(
    parse_expr(quote(a <- parameter(type = TRUE)), NULL, NULL),
    "type' must be a scalar character, but was 'FALSE'")
  expect_error(
    parse_expr(quote(a <- parameter(type = "int")), NULL, NULL),
    "Invalid value 'int for argument 'type'")
})


test_that("can't differentiate non-real parameters", {
  expect_error(
    parse_expr(quote(a <- parameter(type = "integer", differentiate = TRUE)),
               NULL, NULL),
    "Differentiable parameters must have 'type = \"real\"'")
  expect_error(
    parse_expr(quote(a <- parameter(type = "logical", differentiate = TRUE)),
               NULL, NULL),
    "Differentiable parameters must have 'type = \"real\"'")
})

test_that("can specify min/max values", {
  res <- parse_expr(quote(a <- parameter(min = 1, max = 2)), NULL, NULL)
  expect_equal(res$rhs$args$min, 1)
  expect_equal(res$rhs$args$max, 2)
})


test_that("can't use min/max for logical parameters", {
  expect_error(
    parse_expr(quote(a <- parameter(type = "logical", min = 1)), NULL, NULL),
    "'min' cannot be used with 'type = \"logical\"'")
  expect_error(
    parse_expr(quote(a <- parameter(type = "logical", max = 1)), NULL, NULL),
    "'max' cannot be used with 'type = \"logical\"'")
})


test_that("min/max must be numbers", {
  expect_error(
    parse_expr(quote(a <- parameter(min = x)), NULL, NULL),
    "'min' must be a number")
  expect_error(
    parse_expr(quote(a <- parameter(max = NA_real_)), NULL, NULL),
    "'max' must be a number")
})


test_that("min/max must leave a range of valid values", {
  expect_error(
    parse_expr(quote(a <- parameter(min = 10, max = 1)), NULL, NULL),
    "'min' must be smaller than 'max'")
})


test_that("sensible error if parameters are incorrectly specified", {
  expect_error(
    parse_expr(quote(a <- parameter(other = TRUE)), NULL, NULL),
    "Invalid call to 'parameter()'",
    fixed = TRUE)
})


test_that("parse data assignment", {
  res <- parse_expr(quote(d <- data()), NULL, NULL)
  expect_equal(res$special, "data")
  expect_equal(res$lhs$name, "d")
  expect_equal(res$rhs, list(type = "data"))
})


test_that("data calls must be very simple", {
  expect_error(
    parse_expr(quote(d <- data(integer = TRUE)), NULL, NULL),
    "Calls to 'data()' must have no arguments",
    fixed = TRUE)
  expect_error(
    parse_expr(quote(deriv(d) <- data()), NULL, NULL),
    "Calls to 'data()' must be assigned to a symbol",
    fixed = TRUE)
})


test_that("parameter calls must be assigned to a symbol", {
  expect_error(
    parse_expr(quote(deriv(d) <- parameter()), NULL, NULL),
    "Calls to 'parameter()' must be assigned to a symbol",
    fixed = TRUE)
  expect_error(
    parse_expr(quote(initial(x) <- parameter()), NULL, NULL),
    "Calls to 'parameter()' must be assigned to a symbol",
    fixed = TRUE)
})


test_that("interpolate calls must be assigned to a symbol", {
  expect_error(
    parse_expr(quote(deriv(d) <- interpolate(at, ay, "constant")), NULL, NULL),
    "Calls to 'interpolate()' must be assigned to a symbol",
    fixed = TRUE)
})


test_that("Reject unclassifiable expressions", {
  expect_error(
    parse_expr(quote(a), NULL, NULL),
    "Unclassifiable expression")
})


test_that("can parse expressions that involve stochastics", {
  res <- parse_expr(quote(a <- Normal(0, 1)), NULL, NULL)
  expect_null(res$special)
  expect_equal(res$lhs, list(name = "a", array = NULL, depends = NULL))
  expect_identical(
    res$rhs$expr,
    quote(OdinStochasticCall(sample = "normal", mean = 0)(0, 1)))
  expect_equal(res$rhs$depends,
               list(functions = "Normal", variables = character()))
})


test_that("can parse compound expressions that involve stochastics", {
  res <- parse_expr(quote(a <- Normal(0, 1) + Binomial(n, p)), NULL, NULL)

  expect_identical(
    res$rhs$expr[[2]],
    quote(OdinStochasticCall(sample = "normal", mean = 0)(0, 1)))
  expect_identical(
    res$rhs$expr[[3]],
    quote(OdinStochasticCall(sample = "binomial", mean = n * p)(n, p)))
  expect_equal(res$rhs$depends,
               list(functions = c("+", "Normal", "Binomial"),
                    variables = c("n", "p")))
  expect_true(res$rhs$is_stochastic)

  expect_identical(rewrite_stochastic_to_expectation(res$rhs$expr),
                   quote(0 + n * p))
})


test_that("can parse recursive expressions that involve stochastics", {
  res <- parse_expr(quote(a <- Normal(Poisson(p), Exponential(r))),
                    NULL, NULL)
  expect_equal(res$rhs$expr[[1]]$mean, quote(p))

  res <- parse_expr(quote(a <- Normal(Poisson(Exponential(r)), 1)),
                    NULL, NULL)
  expect_equal(res$rhs$expr[[1]]$mean, quote(1 / r))
})


test_that("throw sensible error if stochastic parse fails", {
  expect_error(
    parse_expr(quote(a <- Normal(mu = 0, sigma = 1)), NULL, NULL),
    "Invalid call to 'Normal()'", fixed = TRUE)
})


test_that("check args", {
  expect_error(
    parse_expr(quote(a <- round(a, 3)), NULL, NULL),
    "Invalid call to 'round':")
  expect_error(
    parse_expr(quote(a <- round(y = 2)), NULL, NULL),
    "Invalid call to 'round'")
  expect_error(
    parse_expr(quote(a <- sin(y = 2)), NULL, NULL),
    "Calls to 'sin' may not have any named arguments")
})


test_that("check argument number", {
  ## Once names on primative calls is fixed, we'll have very few of
  ## these left and may remove the check
  expect_error(
    parse_expr(quote(a <- sin(1, 2)), NULL, NULL),
    "Invalid call to 'sin': incorrect number of arguments")
  expect_error(
    parse_expr(quote(a <- `+`(1, 2, 3)), NULL, NULL),
    "Invalid call to '+': incorrect number of arguments",
    fixed = TRUE)
})


test_that("check functions against valid list", {
  expect_error(
    parse_expr(quote(a <- sin(1) + pgamma(2)), NULL, NULL),
    "Unsupported function 'pgamma'")
})


test_that("allow parsing coercion functions", {
  expr <- quote(a <- b + as.integer(c))
  res <- parse_expr(expr, NULL, NULL)
  expect_equal(res$rhs$expr, expr[[3]])
  expect_equal(res$rhs$depends, list(functions = c("+", "as.integer"),
                                     variables = c("b", "c")))
})


test_that("check that coersion functions use simple calls only", {
  expect_error(
    parse_expr(quote(a <- as.integer(x = 1)), NULL, NULL),
    "Calls to 'as.integer' may not have any named arguments")
  expect_error(
    parse_expr(quote(a <- as.logical(1, 2)), NULL, NULL),
    "Invalid call to 'as.logical'")
})


test_that("give nice error if assigning to nonsense array", {
  expect_error(
    parse_expr(quote(1[2] <- 1), NULL, NULL),
    "Invalid target '1' on the lhs of array assignment")
  expect_error(
    parse_expr(quote(update(1[2]) <- 1), NULL, NULL),
    "Invalid target '1' within 'update()' on the lhs of array assignment",
    fixed = TRUE)
})


test_that("give nice error if parameter used in incorrect location", {
  expect_error(
    parse_expr(quote(x <- 1 + parameter(2)), NULL, NULL),
    "'parameter()' must be the only call on the rhs",
    fixed = TRUE)
})


test_that("give nice error if interpolate used incorrectly", {
  expect_error(
    parse_expr(quote(a <- interpolate() + 1), NULL, NULL),
    "'interpolate()' must be the only call on the rhs",
    fixed = TRUE)
})

test_that("give nice errors if special functions used in incorrect location", {
  expect_error(
    parse_expr(quote(y <- update(x)), NULL, NULL),
    "Special function 'update' is not allowed on the rhs")
  expect_error(
    parse_expr(quote(y <- initial(x)), NULL, NULL),
    "Special function 'initial' is not allowed on the rhs")
  expect_error(
    parse_expr(quote(y <- 2 / deriv(x)), NULL, NULL),
    "Special function 'deriv' is not allowed on the rhs")
})


test_that("if expressions must have else clauses", {
  expect_error(
    parse_expr(quote(y <- if (foo) 1), NULL, NULL),
    "All 'if' statements must have an 'else' clause")
})


test_that("disallow function definitions in odin code", {
  expect_error(
    parse_expr(quote(fn <- function(x) x + 1), NULL, NULL),
    "Can't use 'function' within odin code")
})


test_that("sensible error if silly values for functions provided", {
  expect_error(
    parse_expr(quote(x <- 1(2)), NULL, NULL),
    "Unsupported expression used as function '1()'",
    fixed = TRUE)
  expect_error(
    parse_expr(quote(x <- f(x)(2)), NULL, NULL),
    "Unsupported expression used as function 'f(x)()'",
    fixed = TRUE)
})


test_that("Can't assign to reserved names", {
  expect_error(parse_expr(quote(i <- 1), NULL, NULL),
               "Can't assign to reserved name 'i'")
  expect_error(parse_expr(quote(deriv <- 1), NULL, NULL),
               "Can't assign to reserved name 'deriv'")
  expect_error(parse_expr(quote(time <- 1), NULL, NULL),
               "Can't assign to reserved name 'time'")
  expect_error(parse_expr(quote(dim <- 1), NULL, NULL),
               "Can't assign to reserved name 'dim'")
  expect_error(parse_expr(quote(parameter <- 1), NULL, NULL),
               "Can't assign to reserved name 'parameter'")
})


test_that("Can't assign to names with reserved prefixes", {
  expect_error(parse_expr(quote(odin_x <- 1), NULL, NULL),
               "Invalid name 'odin_x' starts with reserved prefix 'odin'")
  expect_error(parse_expr(quote(adjoint_x <- 1), NULL, NULL),
               "Invalid name 'adjoint_x' starts with reserved prefix 'adjoint'")
  expect_error(parse_expr(quote(dim_x <- 1), NULL, NULL),
               "Invalid name 'dim_x' starts with reserved prefix 'dim'")
})


test_that("can parse user-sized arrays", {
  res <- parse_expr(quote(dim(x) <- parameter(rank = 1)), NULL, NULL)
  expect_equal(res$rhs$value, list(NULL))
  expect_true(res$rhs$is_user_sized)
  res <- parse_expr(quote(dim(x) <- parameter(rank = 3)), NULL, NULL)
  expect_equal(res$rhs$value, list(NULL, NULL, NULL))
  expect_true(res$rhs$is_user_sized)
})


test_that("require a rank argument if arrays are user sized", {
  expect_error(
    parse_expr(quote(dim(x) <- parameter()), NULL, NULL),
    "When using 'dim() <- parameter(...)', a 'rank' argument is required",
    fixed = TRUE)
})


test_that("require that a rank is a size if given", {
  expect_error(
    parse_expr(quote(dim(x) <- parameter(rank = a)), NULL, NULL),
    "'rank' must be a scalar size, if given")
})


test_that("calls to dim require at least one arg", {
  expect_error(
    parse_expr(quote(dim() <- 1), NULL, NULL),
    "Invalid call to 'dim()' on lhs; no variables given",
    fixed = TRUE)
})


test_that("calls to dim require symbolic args", {
  expect_error(
    parse_expr(quote(dim(1) <- 1), NULL, NULL),
    "Invalid call to 'dim()' on lhs; '1' is not a symbol",
    fixed = TRUE)
  expect_error(
    parse_expr(quote(dim(a, 1, b) <- 1), NULL, NULL),
    "Invalid call to 'dim()' on lhs; '1' is not a symbol",
    fixed = TRUE)
  expect_error(
    parse_expr(quote(dim(a, "b", c) <- 1), NULL, NULL),
    "Invalid call to 'dim()' on lhs; '\"b\"' is not a symbol",
    fixed = TRUE)
})


test_that("require that rank argument is missing generally for parameters", {
  expect_error(
    parse_expr(quote(a <- parameter(rank = 4)), NULL, NULL),
    "Invalid use of 'rank' argument in 'parameter()'",
    fixed = TRUE)
  expect_error(
    parse_expr(quote(a <- parameter(rank = a)), NULL, NULL),
    "Invalid use of 'rank' argument in 'parameter()'",
    fixed = TRUE)
})


test_that("require that dim argument rhs is array name", {
  expect_error(
    parse_expr(quote(dim(a) <- dim(b[])), NULL, NULL),
    "When using 'dim()' on the right-hand-side, it takes only an array name",
    fixed = TRUE)
})


test_that("helpful error message on misspelt special function", {
  err <- expect_error(
    parse_expr(quote(udate(x) <- 1), NULL, NULL),
    "Invalid special function 'udate()' on the lhs of assignment",
    fixed = TRUE)
  expect_match(conditionMessage(err), "Did you mean 'update()'", fixed = TRUE)

  err <- expect_error(
    parse_expr(quote(udate(x[]) <- 1), NULL, NULL),
    "Invalid special function 'udate()' on the lhs of assignment",
    fixed = TRUE)
  expect_match(conditionMessage(err), "Did you mean 'update()'", fixed = TRUE)
})


test_that("prevent incorrect order of array/special nesting", {
  err <- expect_error(
    parse_expr(quote(update(x)[] <- 1), NULL, NULL),
    "Invalid array access outside of special function 'update()'",
    fixed = TRUE)
  expect_match(
    conditionMessage(err),
    "Did you mean 'update(x[...])' rather than 'update(x)[...]'",
    fixed = TRUE)
})


test_that("prevent nested special calls", {
  expect_error(
    parse_expr(quote(initial(initial(x)) <- 1), NULL, NULL),
    "Invalid nested special lhs function 'initial' within 'initial'",
    fixed = TRUE)
  expect_error(
    parse_expr(quote(initial(update(x)) <- 1), NULL, NULL),
    "Invalid nested special lhs function 'update' within 'initial'",
    fixed = TRUE)
})


test_that("parse min as a reduction", {
  res <- parse_expr(quote(a <- min(x)), NULL, NULL)
  expect_equal(res$rhs$expr,
               quote(OdinReduce("min", "x", index = NULL)))
  expect_equal(res$rhs$depends,
               list(functions = "min", variables = "x"))
})


test_that("parse min as a 2-arg function", {
  res <- parse_expr(quote(a <- min(x, y)), NULL, NULL)
  expect_equal(res$rhs$expr, quote(min(x, y)))
  expect_equal(res$rhs$depends,
               list(functions = "min", variables = c("x", "y")))

  expect_error(
    parse_expr(quote(a <- min(x, y, z)), NULL, NULL),
    "Invalid call to 'min': incorrect number of arguments")
})


test_that("parse max as a reduction", {
  res <- parse_expr(quote(a <- max(x)), NULL, NULL)
  expect_equal(res$rhs$expr,
               quote(OdinReduce("max", "x", index = NULL)))
  expect_equal(res$rhs$depends,
               list(functions = "max", variables = "x"))
})


test_that("parse max as a 2-arg function", {
  res <- parse_expr(quote(a <- max(x, y)), NULL, NULL)
  expect_equal(res$rhs$expr, quote(max(x, y)))
  expect_equal(res$rhs$depends,
               list(functions = "max", variables = c("x", "y")))
})


test_that("can parse call to browser()", {
  res <- parse_expr(quote(browser("update")), NULL, NULL)
  expect_equal(res$type, "browser")
  expect_equal(res$special, "browser")
  expect_equal(res$phase, "update")
  expect_null(res$when, NULL)
})


test_that("can parse conditional call to browser()", {
  res <- parse_expr(quote(browser("deriv", time > 4)), NULL, NULL)
  expect_equal(res$type, "browser")
  expect_equal(res$special, "browser")
  expect_equal(res$phase, "deriv")
  expect_equal(res$when, quote(time > 4))
})


test_that("error for invalid call to browser", {
  expect_error(
    parse_expr(quote(browser("deriv", time > 4, TRUE)), NULL, NULL),
    "Failed to parse 'browser()' call",
    fixed = TRUE)
})


test_that("error for invalid value for phase in browser call", {
  expect_error(
    parse_expr(quote(browser("other", time > 4)), NULL, NULL),
    "Invalid value for 'phase' argument to 'browser()'",
    fixed = TRUE)
})

test_that("Can parse expression involving pi on rhs", {
  res <- parse_expr(quote(a <- sin(180 / pi)), NULL, NULL)
  expect_true(res$rhs$depends$variables == "pi")
})

test_that("Can't parse with pi on lhs", {
  expect_error(
    parse_expr(quote(pi <- 3), NULL, NULL),
    "Do not use `pi` on the left-hand-side of an expression",
    fixed = TRUE)
  expect_error(
    parse_expr(quote(initial(pi) <- 1), NULL, NULL),
    "Do not use `pi` on the left-hand-side of an expression",
    fixed = TRUE)
  expect_error(
    parse_expr(quote(update(pi) <- 1), NULL, NULL),
    "Do not use `pi` on the left-hand-side of an expression",
    fixed = TRUE)
  expect_error(
    parse_expr(quote(dim(pi) <- c(2,2)), NULL, NULL),
    "Do not use `pi` on the left-hand-side of an expression",
    fixed = TRUE)
  expect_error(
    parse_expr(quote(pi ~ Normal(1, 1)), NULL, NULL),
    "Do not use `pi` on the left-hand-side of an expression",
    fixed = TRUE)
})


test_that("top-level calls to as.integer create integer variables", {
  res <- parse_expr(quote(a <- as.integer(b)), NULL, NULL)
  expect_equal(res$lhs$storage_type, "int")
})


test_that("top-level calls to as.logical create logical variables", {
  res <- parse_expr(quote(a <- as.logical(b)), NULL, NULL)
  expect_equal(res$lhs$storage_type, "bool")
})


test_that("disallow use of NA", {
  expect_error(
    parse_expr(quote(a <- NA), NULL, NULL),
    "Cannot use 'NA' within expressions")
  expect_error(
    parse_expr(quote(a <- NA_real_), NULL, NULL),
    "Cannot use 'NA_real_' within expressions")
})


test_that("length is treated as special dependency", {
  res <- parse_expr(quote(x <- length(b)), NULL, NULL)
  expect_equal(res$rhs$depends$variables, "dim_b")
  res <- parse_expr(quote(dim(a) <- length(b)), NULL, NULL)
  expect_equal(res$rhs$depends$variables, "dim_b")
})


test_that("parse sum within compare", {
  res <- parse_expr(quote(d ~ Poisson(sum(y))), NULL, NULL)
  expect_equal(res$rhs$args,
               list(quote(d),
                    quote(OdinReduce("sum", "y", index = NULL))))
})


test_that("parse output", {
  res <- parse_expr(quote(output(x) <- TRUE), NULL, NULL)
  expect_equal(res$lhs$name, "x")
  expect_equal(res$rhs$expr, TRUE)

  res <- parse_expr(quote(output(x) <- 2 * y), NULL, NULL)
  expect_equal(res$lhs$name, "x")
  expect_equal(res$rhs$expr, quote(2 * y))
})
