## this is pretty gross, but it will at least allow some stability
## over the next few versions.
test_that("can parse nontrivial system with adjoint", {
  dat <- odin_parse({
    p_IR <- 1 - exp(-gamma * dt)
    S0 <- 1000
    N <- S + I + R
    p_inf <- beta * I / N * dt
    p_SI <- 1 - exp(-p_inf)
    n_SI <- S * p_SI # Binomial(S, p_SI)
    n_IR <- I * p_IR # Binomial(I, p_IR)
    update(S) <- S - n_SI
    update(I) <- I + n_SI - n_IR
    update(R) <- R + n_IR
    update(cases_inc) <- if (time %% 1 == 0) n_SI else cases_inc + n_SI
    update(cases_cumul) <- cases_cumul + n_SI
    initial(S) <- S0
    initial(I) <- I0
    initial(R) <- 0
    initial(cases_inc) <- 0
    initial(cases_cumul) <- 0
    beta <- parameter(0.2, differentiate = TRUE)
    gamma <- parameter(0.1, differentiate = TRUE)
    I0 <- parameter(10, differentiate = TRUE)
    cases_observed <- data()
    compare(cases_observed) ~ Poisson(cases_inc)
  })

  expect_type(dat$adjoint, "list")
  expect_setequal(names(dat$adjoint), c("update", "compare", "initial"))

  expect_equal(dat$adjoint$update$unpack, c("S", "I", "R"))
  expect_equal(dat$adjoint$update$unpack_adjoint,
               c("adj_S", "adj_I", "adj_R",
                 "adj_cases_inc", "adj_cases_cumul",
                 "adj_beta", "adj_gamma", "adj_I0"))
  expect_equal(dat$adjoint$update$equations,
               c("N", "p_IR", "p_inf", "p_SI"))
  expect_length(dat$adjoint$update$adjoint, 14)
  expect_equal(dat$adjoint$update$adjoint[[1]]$lhs$name, "adj_n_SI")
  expect_equal(dat$adjoint$update$adjoint[[1]]$rhs$expr,
               quote(-adj_S + adj_I + adj_cases_inc + adj_cases_cumul))
})
