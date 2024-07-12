SPECIAL_LHS <- c(
  "initial", "deriv", "update", "output", "dim", "config", "compare")

COMPARE <- list(
  Normal = function(mean, sd) {},
  Poisson = function(lambda) {})

STOCHASTIC <- list(
  Binomial = function(size, prob) {})

FUNCTIONS <- list(
  exp = function(x) {})
