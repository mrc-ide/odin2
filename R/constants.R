SPECIAL_LHS <- c(
  "initial", "deriv", "update", "output", "dim", "config", "compare")

FUNCTIONS <- list(
  exp = function(x) {})

## This list needs to come from mcstate2, at some point, we have that
## as mcstate2:::dsl_distributions so just need to export it.  It
## should be the things that are able to support stochastics.  Perhaps
## we provide a table?  For now just hard code it here...
FUNCTIONS_STOCHASTIC <- c("Binomial", "Exponential", "Gamma", "Normal",
                          "Poisson", "Uniform")
