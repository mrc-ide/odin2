test_that("Error if undefined variables are used", {
  expect_error(
    odin_parse({
      initial(x) <- a
      update(x) <- b
      b <- 1 + y
    }),
    "Unknown variables used in odin code: 'a' and 'y'")
})


test_that("Error if undefined variables used in dead code", {
  ## We might relax this one later, but it's probably a good thing
  expect_error(
    odin_parse({
      initial(x) <- 1
      update(x) <- 1
      a <- b + 1
    }),
    "Unknown variable used in odin code: 'b'")
})
