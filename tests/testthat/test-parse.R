test_that("can parse trivial system", {
  res <- odin_parse({
    initial(x) <- 0
    update(x) <- 0
  })
  expect_equal(res$time, "discrete")
  expect_equal(res$class, "odin")
  expect_equal(res$variables, "x")
  expect_equal(nrow(res$parameters), 0)
  expect_equal(nrow(res$data), 0)
})


test_that("can parse dust MVP system", {
  ## Silly system that will work towards being one of the MVPs for
  ## compilation to dust; contains basically everything we are
  ## interested in.
  res <- odin_parse({
    z <- x + a
    update(x) <- x + z * 2 + p
    a <- 1 * b
    b <- parameter()
    p <- parameter(constant = TRUE)
    initial(x) <- 0
    d <- data()
    d ~ Normal(x, 1)
  })

  expect_equal(res$time, "discrete")
  expect_equal(res$class, "odin")
  expect_equal(res$variables, "x")
  expect_equal(res$parameters,
               data_frame(name = c("b", "p"),
                          type = "real_type",
                          differentiate = FALSE,
                          constant = c(FALSE, TRUE)))
  expect_equal(res$data, data_frame(name = "d"))
})


test_that("throw error with context", {
  path <- withr::local_tempfile()
  writeLines(c("initial(x) <- a",
               "update(x) <- x + b",
               "b<-parameter(invalid=TRUE)",
               "a <- 5"),
             path)
  err <- expect_error(
    odin_parse(path),
    "Invalid call to 'parameter()'",
    fixed = TRUE,
    class = "odin_parse_error")
  expect_equal(
    err$src,
    list(list(value = quote(b <- parameter(invalid = TRUE)),
              index = 3,
              start = 3,
              end = 3,
              str = c("b<-parameter(invalid=TRUE)"))))
  expect_match(
    conditionMessage(err),
    "Context:\n3| b<-parameter(invalid=TRUE)",
    fixed = TRUE)
})


test_that("throw error with context where source code unavailable", {
  err <- expect_error(
    odin_parse({
      initial(x) <- a
      update(x) <- x + b
      b <- parameter(invalid = TRUE)
      a <- 5
    }),
    "Invalid call to 'parameter()'",
    fixed = TRUE,
    class = "odin_parse_error")
  expect_match(
    conditionMessage(err),
    "Context:\nb <- parameter(invalid = TRUE)",
    fixed = TRUE)
})


test_that("throw stochastic parse error sensibly", {
  err <- expect_error(
    odin_parse({
      initial(x) <- 0
      update(x) <- Normal(mu = 0, sd = 1)
    }),
    "Invalid call to 'Normal()'",
    fixed = TRUE)
  expect_match(
    conditionMessage(err),
    deparse(quote(update(x) <- Normal(mu = 0, sd = 1))),
    fixed = TRUE)
})


test_that("can parse system that resets", {
  d <- odin_parse({
    update(x) <- x + 1
    initial(x, zero_every = 4) <- 0
    update(y) <- y + 1
    initial(y) <- 0
  })
  expect_equal(d$zero_every, list(x = 4))
})


test_that("zero_reset requires that initial conditions are zero", {
  expect_error(
    odin_parse({
      update(x) <- x + 1
      initial(x, zero_every = 1) <- 1
    }),
    "Initial condition of periodically zeroed variable must be 0")
})


test_that("zero_reset requires an integer argument", {
  expect_error(
    odin_parse({
      update(x) <- x + 1
      initial(x, zero_every = 1.4) <- 1
    }),
    "Argument to 'zero_every' must be an integer")
  expect_error(
    odin_parse({
      update(x) <- x + 1
      initial(x, zero_every = a) <- 1
    }),
    "Argument to 'zero_every' must be an integer")
})


test_that("can parse ode system that resets", {
  d <- odin_parse({
    deriv(x) <- 1
    initial(x, zero_every = 4) <- 0
    deriv(y) <- 1
    initial(y) <- 0
  })
  expect_equal(d$zero_every, list(x = 4))
})


test_that("parse systems that require shared storage", {
  dat <- odin_parse({
    a <- 1
    b <- parameter()
    c <- a + b
    initial(x) <- 1
    update(x) <- x + c
  })

  expect_equal(dat$storage$contents$shared,
               c("a", "b", "c"))
})


test_that("can parse systems that involve arrays in internal", {
  d <- odin_parse({
    initial(x) <- 1
    update(x) <- a[1] + a[2]
    a[] <- Normal(0, 1)
    dim(a) <- 2
  })

  expect_equal(d$storage$location[["a"]], "internal")
  expect_equal(
    d$storage$arrays,
    data_frame(name = "a",
               rank = 1,
               dims = I(list(list(2))),
               size = I(list(2))))
  expect_equal(
    d$equations$a$lhs$array,
    list(list(name = "i", type = "range",
              from = 1, to = quote(OdinDim("a", 1L)))))
})


test_that("can parse systems that involve arrays in shared", {
  d <- odin_parse({
    initial(x) <- 1
    update(x) <- a[1] + a[2] + a[3]
    a[] <- i
    dim(a) <- 3
  })

  expect_equal(d$storage$location[["a"]], "shared")
  expect_equal(
    d$storage$arrays,
    data_frame(name = "a",
               rank = 1,
               dims = I(list(list(3))),
               size = I(list(3))))
  expect_equal(
    d$equations$a$lhs$array,
    list(list(name = "i", type = "range",
              from = 1, to = quote(OdinDim("a", 1L)))))
})


test_that("pack system entirely composed of arrays", {
  d <- odin_parse({
    initial(x[]) <- 1
    update(x[]) <- x[i] * 2
    initial(y[]) <- 1
    update(y[]) <- y[i] / x[i]
    dim(x) <- 2
    dim(y) <- 2
  })
  expect_equal(
    d$storage$packing$state,
    data_frame(name = c("x", "y"),
               rank = 1,
               dims = I(list(list(2), list(2))),
               size = I(list(2, 2)),
               offset = I(list(0, 2))))
})


test_that("pack system of mixed arrays and scalars", {
  d <- odin_parse({
    initial(x[]) <- 1
    update(x[]) <- x[i] * 2
    initial(y) <- 1
    update(y) <- y / (x[1] + x[2])
    dim(x) <- 2
  })
  expect_equal(
    d$storage$packing$state,
    data_frame(name = c("x", "y"),
               rank = c(1, 0),
               dims = I(list(list(2), NULL)),
               size = I(list(2, 1)),
               offset = I(list(0, 2))))
})


test_that("can use sqrt", {
  ## Reported missing by Marc in 0.1.2
  expect_no_error(
    odin_parse({
      update(x) <- sqrt(x)
      initial(x) <- 100
    }))
})


test_that("parse simple stochastic system", {
  dat <- odin_parse({
    update(x) <- Normal(x, 1)
    initial(x) <- 0
  })
  expect_length(dat$phases$update$variables, 1)
  expect_equal(dat$phases$update$variables[[1]]$lhs$name, "x")
  expect_equal(dat$phases$update$unpack, "x")
})


test_that("parse system with adjoint", {
  dat <- odin_parse({
    update(x) <- x + a
    initial(x) <- 1
    a <- parameter(differentiate = TRUE)
    p <- exp(x)
    d <- data()
    d ~ Poisson(p)
  })

  expect_equal(dat$adjoint$update$adjoint[[1]]$rhs$expr, quote(adj_x))
})


test_that("can cope with array equations involving multiple assignment", {
  d <- odin_parse({
    initial(x) <- 1
    update(x) <- b
    b <- a[1] + a[2]
    n <- parameter(type = "integer", constant = TRUE)
    dim(a) <- n
    a[1] <- 1
    a[2] <- Normal(0, 1)
  })
  ## One copy of 'a' in the update equations
  expect_equal(d$phases$update$equations,
               c("a", "b"))
  ## Correct topological order, with two copies of 'a'
  expect_equal(names(d$equations),
               c("n", "dim_a", "a", "a", "b"))
  expect_equal(d$equations[[3]]$lhs$array,
               list(list(name = "i", type = "single", at = 1)))
  expect_equal(d$equations[[4]]$lhs$array,
               list(list(name = "i", type = "single", at = 2)))
})


test_that("allow multline array statement within update", {
  d <- odin_parse({
    initial(x[]) <- 1
    update(x[1]) <- a[1]
    update(x[2]) <- a[2]
    dim(a) <- 2
    a[] <- Normal(0, 1)
    dim(x) <- 2
  })

  expect_equal(d$phases$update$equations, "a")
  ## Correct topological order, with two copies of 'a'
  expect_equal(names(d$equations),
               c("dim_a", "dim_x", "a"))
  expect_length(d$phases$update$variables, 2)
  expect_equal(d$phases$update$variables[[1]]$lhs$array,
               list(list(name = "i", type = "single", at = 1)))
  expect_equal(d$phases$update$variables[[2]]$lhs$array,
               list(list(name = "i", type = "single", at = 2)))
})


test_that("can write self-referential multipart equations", {
  d <- odin_parse({
    initial(x) <- 1
    update(x) <- x + a[n]
    n <- parameter(type = "integer", constant = TRUE)
    a[1] <- 1
    a[2] <- 1
    a[3:length(a)] <- a[i - 2] + a[i - 1]
    dim(a) <- n
  })

  expect_equal(d$phases$build_shared$equations,
               c("n", "dim_a", "a"))
  expect_equal(names(d$equations),
               c("n", "dim_a", "a", "a", "a"))
  expect_equal(d$equations[[3]]$lhs$array,
               list(list(name = "i", type = "single", at = 1)))
  expect_equal(d$equations[[4]]$lhs$array,
               list(list(name = "i", type = "single", at = 2)))
  expect_equal(d$equations[[5]]$lhs$array,
               list(list(name = "i", type = "range", from = 3,
                         to = quote(length(a)))))
  expect_equal(d$equations[[5]]$rhs$depends$variables, c("a", "dim_a"))
})


test_that("non-arrays cannot be self-referential", {
  expect_error(
    odin_parse({
      initial(x) <- 1
      update(x) <- b
      b <- b + b
    }),
    "Equation 'b' cannot reference itself")
})


test_that("error if arrays have non-constant dimension", {
  err <- expect_error(
    odin_parse({
      initial(x) <- 1
      update(x) <- sum(a)
      a[] <- 1
      dim(a) <- n
      n <- parameter(type = "integer")
    }),
    "Dimensions of arrays are not determined at initial creation")
  expect_match(
    err$body[[1]],
    "'a' is determined at stage 'parameter_update', it depends on 'n'",
    fixed = TRUE)
  expect_match(
    err$body[[2]],
    "Try adding `constant = TRUE` into the 'parameter()' call for 'n'",
    fixed = TRUE)
})


test_that("error if arrays have non-constant dimension", {
  err <- expect_error(
    odin_parse({
      initial(x) <- 1
      update(x) <- sum(a)
      a[] <- 1
      dim(a) <- n
      n <- Poisson(2)
    }),
    "Dimensions of arrays are not determined at initial creation")
  expect_match(
    err$body[[1]],
    "'a' is determined at stage 'time', it depends on 'n' (time)",
    fixed = TRUE)
  expect_length(err$body, 1)
})


test_that("only arrays can be duplicated", {
  err <- expect_error(
    odin_parse({
      update(x) <- a
      initial(x) <- 0
      a <- 1
      a <- 2
    }),
    "Only arrays can be assigned over multiple statements, but 'a' is")
})


test_that("only arrays can be duplicated", {
  expect_error(
    odin_parse({
      update(x) <- a
      initial(x) <- 0
      a <- 1
      a <- 2
    }),
    "Only arrays can be assigned over multiple statements, but 'a' is")
})


test_that("multline array equations must be contiguous", {
  expect_error(
    odin_parse({
      update(x) <- sum(a)
      initial(x) <- 0
      a[1] <- 1
      b <- 2
      a[2] <- b
      dim(a) <- 2
    }),
    "Multiline array equations must be contiguous statements, but 'a'")
})


test_that("equations cannot shadow variables", {
  expect_error(
    odin_parse({
      initial(x) <- 1
      update(x) <- 1
      x <- 2
    }),
    "Equation uses name belonging to variable: 'x'")

  expect_error(
    odin_parse({
      initial(x) <- 1
      update(x) <- 1
      initial(y) <- 1
      update(y) <- z
      x <- 2
      y <- 3
      z <- 4
    }),
    "Equations use names belonging to variables: 'x' and 'y'")
})
