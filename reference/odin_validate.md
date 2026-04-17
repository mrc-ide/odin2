# Validate odin code

Validate odin code. This is primarily intended for use within other
applications.

## Usage

``` r
odin_validate(
  expr,
  input_type = NULL,
  compatibility = NULL,
  check_bounds = NULL
)
```

## Arguments

- expr:

  Odin code as the path to a file (a string), a character vector of
  code, or as an expression (typically within braces
  [`{}`](https://rdrr.io/r/base/Paren.html)).

- input_type:

  An optional string describing the type of input for `expr` - must be
  one of `file`, `text` or `expression`. If given, this skips the type
  detection logic and odin will throw an error if the wrong type of
  input is given. Using this may be beneficial in programmatic
  environments.

- compatibility:

  Compatibility mode to use. Valid options are "warning", which updates
  code that can be fixed, with warnings, and "error", which will error.
  The option "silent" will silently rewrite code, but this is not
  recommended for general use as eventually the compatibility mode will
  be removed (this option is primarily intended for comparing output of
  odin1 and odin2 models against old code). The default, `NULL`,
  currently corresponds to `warning`.

- check_bounds:

  Control over static array bounds checking. This is enabled by default,
  but is prone to false positives, erroring where a read or write
  appears out of bounds but is actually ok. This argument exists to
  allow you to disable the check and compile the model anyway. Future
  versions may allow specific lines to be ignored, which will provide
  finer control and allow you to use the bits of the checks that are
  actually helpful. You can also pass `TRUE` here to mean "error" or
  `FALSE` to mean "disabled".

## Value

A list with elements:

- `success`: boolean, `TRUE` if validation was successful

- `result`: Metadata about the model; see Details above for the format.

- `error`: Either `NULL` (if `success` is `TRUE`) or an error; see
  Details above for interpreting this value.

- `compatibility`: A `data.frame` of compatibility issues. This is
  formatted similarly to `src` within `error` (see above), but also
  includes `type` (a code for the compatibility issue), `description` (a
  human-readable description of the issue), `original` (the original
  expression) and `value` (the final expression).

The intention is that we won't throw generally from this function.

## Result

On successful validation, we return a list with metadata about the
model. Currently this contains:

- `time`: The time mode of the model (a string of either "discrete" or
  "continuous")

- `parameters`: A `data.frame` describing parameters. Currently the only
  column is `name`.

- `variables`: A `data.frame` describing the model variables. Currently
  the only column is `name`.

- `data`: A `data.frame` describing data used by the model (if it
  supports this). Currently the only column is `name`.

## Errors

Most errors will have class `odin_parse_error`. These will print context
information if rethrown. They have fields:

- `message`: The headline error message

- `code`: The odin error code, as listed in
  [`vignette("errors")`](https://mrc-ide.github.io/odin2/articles/errors.md),
  and used by
  [odin_error_explain](https://mrc-ide.github.io/odin2/reference/odin_error_explain.md)

- `src`: Source information about the error. This is a `data.frame` with
  columns `index` (the expression number), `expr` (a list column with
  the expression), `start` (the starting line; possibly `NA`), `end`
  (the finishing line; possibly `NA`), `str` (the string containing the
  literal value of the expression; possibly `NA`) and `migrated` (a
  logical, indicating if the source has been automatically migrated from
  odin1 code). If any of `start`, `end` or `str` is `NA`, all will be,
  for all rows.

You can get the full rendered message using
[`conditionMessage()`](https://rdrr.io/r/base/conditions.html) on the
error object.

## Examples

``` r
# A successful validation:
odin_validate({
  initial(x) <- 1
  deriv(x) <- a
  a <- parameter()
})
#> $success
#> [1] TRUE
#> 
#> $error
#> NULL
#> 
#> $result
#> $result$time
#> [1] "continuous"
#> 
#> $result$variables
#>   name
#> 1    x
#> 
#> $result$parameters
#>   name
#> 1    a
#> 
#> $result$data
#> [1] name
#> <0 rows> (or 0-length row.names)
#> 
#> 
#> $compatibility
#> NULL
#> 

# A failure:
odin_validate({
  initial(x) <- 1
  deriv(x) <- a
})
#> $success
#> [1] FALSE
#> 
#> $error
#> <error/odin_parse_error>
#> Error in `odin_validate()`:
#> ! Unknown variable used in odin code: 'a'
#> → Context:
#> deriv(x) <- a
#> ℹ For more information, run `odin2::odin_error_explain("E2006")`
#> ---
#> Backtrace:
#>      ▆
#>   1. └─pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)
#>   2.   └─pkgdown::build_site(...)
#>   3.     └─pkgdown:::build_site_local(...)
#>   4.       └─pkgdown::build_reference(...)
#>   5.         ├─pkgdown:::unwrap_purrr_error(...)
#>   6.         │ └─base::withCallingHandlers(...)
#>   7.         └─purrr::map(...)
#>   8.           └─purrr:::map_("list", .x, .f, ..., .progress = .progress)
#>   9.             ├─purrr:::with_indexed_errors(...)
#>  10.             │ └─base::withCallingHandlers(...)
#>  11.             ├─purrr:::call_with_cleanup(...)
#>  12.             └─pkgdown (local) .f(.x[[i]], ...)
#>  13.               ├─base::withCallingHandlers(...)
#>  14.               └─pkgdown:::data_reference_topic(...)
#>  15.                 └─pkgdown:::run_examples(...)
#>  16.                   └─pkgdown:::highlight_examples(code, topic, env = env)
#>  17.                     └─downlit::evaluate_and_highlight(...)
#>  18.                       └─evaluate::evaluate(code, child_env(env), new_device = TRUE, output_handler = output_handler)
#>  19.                         ├─base::withRestarts(...)
#>  20.                         │ └─base (local) withRestartList(expr, restarts)
#>  21.                         │   ├─base (local) withOneRestart(withRestartList(expr, restarts[-nr]), restarts[[nr]])
#>  22.                         │   │ └─base (local) doWithOneRestart(return(expr), restart)
#>  23.                         │   └─base (local) withRestartList(expr, restarts[-nr])
#>  24.                         │     └─base (local) withOneRestart(expr, restarts[[1L]])
#>  25.                         │       └─base (local) doWithOneRestart(return(expr), restart)
#>  26.                         ├─evaluate:::with_handlers(...)
#>  27.                         │ ├─base::eval(call)
#>  28.                         │ │ └─base::eval(call)
#>  29.                         │ └─base::withCallingHandlers(...)
#>  30.                         ├─base::withVisible(eval(expr, envir))
#>  31.                         └─base::eval(expr, envir)
#>  32.                           └─base::eval(expr, envir)
#>  33.                             └─odin2::odin_validate(...)
#> 
#> $result
#> NULL
#> 
#> $compatibility
#> NULL
#> 

# Migration warnings
odin_validate({
  initial(x)
  deriv(x) <- a
  a <- user()
})
#> $success
#> [1] FALSE
#> 
#> $error
#> <error/odin_parse_error>
#> Error in `odin_validate()`:
#> ! Unclassifiable expression
#> ℹ Expected an assignment (with '<-') or a relationship (with '~')
#> → Context:
#> initial(x)
#> ℹ For more information, run `odin2::odin_error_explain("E1001")`
#> ---
#> Backtrace:
#>      ▆
#>   1. └─pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)
#>   2.   └─pkgdown::build_site(...)
#>   3.     └─pkgdown:::build_site_local(...)
#>   4.       └─pkgdown::build_reference(...)
#>   5.         ├─pkgdown:::unwrap_purrr_error(...)
#>   6.         │ └─base::withCallingHandlers(...)
#>   7.         └─purrr::map(...)
#>   8.           └─purrr:::map_("list", .x, .f, ..., .progress = .progress)
#>   9.             ├─purrr:::with_indexed_errors(...)
#>  10.             │ └─base::withCallingHandlers(...)
#>  11.             ├─purrr:::call_with_cleanup(...)
#>  12.             └─pkgdown (local) .f(.x[[i]], ...)
#>  13.               ├─base::withCallingHandlers(...)
#>  14.               └─pkgdown:::data_reference_topic(...)
#>  15.                 └─pkgdown:::run_examples(...)
#>  16.                   └─pkgdown:::highlight_examples(code, topic, env = env)
#>  17.                     └─downlit::evaluate_and_highlight(...)
#>  18.                       └─evaluate::evaluate(code, child_env(env), new_device = TRUE, output_handler = output_handler)
#>  19.                         ├─base::withRestarts(...)
#>  20.                         │ └─base (local) withRestartList(expr, restarts)
#>  21.                         │   ├─base (local) withOneRestart(withRestartList(expr, restarts[-nr]), restarts[[nr]])
#>  22.                         │   │ └─base (local) doWithOneRestart(return(expr), restart)
#>  23.                         │   └─base (local) withRestartList(expr, restarts[-nr])
#>  24.                         │     └─base (local) withOneRestart(expr, restarts[[1L]])
#>  25.                         │       └─base (local) doWithOneRestart(return(expr), restart)
#>  26.                         ├─evaluate:::with_handlers(...)
#>  27.                         │ ├─base::eval(call)
#>  28.                         │ │ └─base::eval(call)
#>  29.                         │ └─base::withCallingHandlers(...)
#>  30.                         ├─base::withVisible(eval(expr, envir))
#>  31.                         └─base::eval(expr, envir)
#>  32.                           └─base::eval(expr, envir)
#>  33.                             └─odin2::odin_validate(...)
#> 
#> $result
#> NULL
#> 
#> $compatibility
#>   index type                                  description     original
#> 1     3 user Replace calls to 'user()' with 'parameter()' <-, a, u....
#>          value start end  str
#> 1 <-, a, p....    NA  NA <NA>
#> 
```
