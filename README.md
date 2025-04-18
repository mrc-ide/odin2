# odin2 <a href="https://mrc-ide.github.io/odin2/"><img src="man/figures/logo.png" align="right" height="139" alt="odin2 website" /></a>

<!-- badges: start -->
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![R build status](https://github.com/mrc-ide/odin2/workflows/R-CMD-check/badge.svg)](https://github.com/mrc-ide/odin2/actions)
[![Codecov test coverage](https://codecov.io/gh/mrc-ide/odin2/graph/badge.svg)](https://app.codecov.io/gh/mrc-ide/odin2)
<!-- badges: end -->

`odin2` implements a high-level language for describing and implementing ordinary differential equations and difference equations in R.  It provides a "[domain specific language](https://en.wikipedia.org/wiki/Domain-specific_language)" (DSL) which _looks_ like R but is compiled directly to C++, using [`dust2`](https://mrc-ide.github.io/dust2/) to solve your system and to provide an interface to particle filters.  You can then use [`monty`](https://mrc-ide.github.io/monty/) to fit your models using MCMC.

* The DSL is _declarative_ reflecting the mathematical nature of the equations.
* It includes support for equations that involve vectors, matrices and higher dimensional arrays (up to 8!), including a high-level array indexing notation that removes the need for explicit looping.
* Supports both discrete-time (possibly stochastic) models, as well as continuous-time ODE models.
* Interpolation functions can be used to include time-varying quantities into the model (piecewise constant, linear and spline interpolation is supported).
* The equations are analysed before compilation so that parts that do not depend on time are not included in time-dependent calculations.
* Supports user-supplied parameters for any part of the system.
* Supports a large number of mathematical functions (see the [functions vignette](https://mrc-ide.github.io/odin2/articles/functions.html) for a complete list).

## Documentation

* See [the introductory vignette](https://mrc-ide.github.io/odin2/articles/odin2.html) for a tutorial-style introduction to `odin2`
* A [tutorial-style guide](https://mrc-ide.github.io/odin2/articles/fitting.html) to using `odin2` with `dust2` and `monty` to fit models to data
* A [reference-style guide](https://mrc-ide.github.io/odin2/articles/functions.html) to the syntax and supported functions
* If you have used [`odin` version 1](https://mrc-ide.github.io/odin) before, see the [migration guide](https://mrc-ide.github.io/odin2/articles/migrating.html) to see what has changed.
* Because `odin2` compiles to `dust2`, see [its documentation](https://mrc-ide.github.io/dust2) and in particular the [list of functions that you can use](https://mrc-ide.github.io/dust2/reference/index.html)

## Roadmap

The package is currently ready for use for adventurous users.  It will eventually become [`odin`](https://mrc-ide.github.io/odin) and replace that version on CRAN (i.e., it will simply become version 2.0.0 of `odin`, and `odin2` will cease to be a package name that you will see).  It also replaces [odin.dust](https://mrc-ide.github.io/odin.dust).  It exists separately for now to facilitate development and use alongside the original `odin`.

See [this list](https://mrc-ide.github.io/odin2/articles/migrating.html#missing-features) of missing features from version 1 of odin before using.

Over the next few months we will be expanding the automatic differentiation support, generating code for other host languages and improving the ergonomics of the package.

## Installation

Please install from our r-universe:

```r
install.packages(
  "odin2",
  repos = c("https://mrc-ide.r-universe.dev", "https://cloud.r-project.org"))
```

If you prefer, you can install from GitHub with `remotes`:

```r
remotes::install_github("mrc-ide/odin2", upgrade = FALSE)
```

## License

MIT © Imperial College of Science, Technology and Medicine
