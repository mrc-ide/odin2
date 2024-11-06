## We need to use this to make setup of some examples a bit easier
## (two places; the reference docs and the packaging vignette)
example_package <- function(path = tempfile()) {
  desc <- c(
    "Package: example",
    "Title: Example Odin in a Package",
    "Version: 0.0.1",
    "Imports: dust2",
    "LinkingTo: cpp11, dust2, monty",
    "Authors@R: c(person('A', 'Person', role = c('aut', 'cre'),",
    "                     email = 'person@example.com'))",
    "License: CC0")
  ns <- "useDynLib('example', .registration = TRUE)"

  models <- list(
    sir = c(
      "p_IR <- 1 - exp(-gamma * dt)",
      "N <- parameter(1000)",
      "",
      "p_SI <- 1 - exp(-(beta * I / N * dt))",
      "n_SI <- Binomial(S, p_SI)",
      "n_IR <- Binomial(I, p_IR)",
      "",
      "update(S) <- S - n_SI",
      "update(I) <- I + n_SI - n_IR",
      "update(R) <- R + n_IR",
      "",
      "initial(S) <- N - I0",
      "initial(I) <- I0",
      "initial(R) <- 0",
      "",
      "beta <- parameter(0.2)",
      "gamma <- parameter(0.1)",
      "I0 <- parameter(10)"))

  dir_create(path)

  writeLines(desc, file.path(path, "DESCRIPTION"))
  writeLines(ns, file.path(path, "NAMESPACE"))
  dir.create(file.path(path, "inst/odin"), FALSE, TRUE)
  for (nm in names(models)) {
    writeLines(models[[nm]], file.path(path, sprintf("inst/odin/%s.R", nm)))
  }
  invisible(path)
}
