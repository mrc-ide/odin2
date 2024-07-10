SPECIAL_LHS <- c(
  "initial", "deriv", "update", "output", "dim", "config", "compare")
TIME <- "t"
DT <- "dt"

## TODO: get netgative binomial here so we can explore the two forms;
## I know I put that somewhere.  For now we might assume all are the same?
COMPARE <- list(
  Normal = function(mean, sd) {},
  Poisson = function(lambda) {})

STOCHASTIC <- list(
  Binomial = function(size, prob) {})

FUNCTIONS <- list(
  exp = function(x) {})
