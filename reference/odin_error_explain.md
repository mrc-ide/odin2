# Explain an odin error

Explain error codes produced by odin. When odin fails to parse your code
(e.g., via [`odin()`](https://mrc-ide.github.io/odin2/reference/odin.md)
or
[`odin_validate()`](https://mrc-ide.github.io/odin2/reference/odin_validate.md))
it will return an error with a code. You can use `odin_error_explain` to
get more information on that code. By default we will print an
explanation to the screen, but you can control this behaviour via the
`how` argument. All error codes can be found in
[`vignette("errors")`](https://mrc-ide.github.io/odin2/articles/errors.md).

## Usage

``` r
odin_error_explain(code, how = "pretty")
```

## Arguments

- code:

  The error code, as a string, in the form `Exxxx` (a capital "E"
  followed by four numbers)

- how:

  How to explain the error. Options are `pretty` (render pretty text in
  the console), `plain` (display plain text in the console) and `link`
  (browse to the online help).

## Value

Nothing, this is called for its side effect only

## Examples

``` r
odin_error_explain("E1006")
#> 
#> ── E1006 ───────────────────────────────────────────────────────────────────────
#> Invalid call to the `parameter()` function, used on the rhs of an assignment.
#> If this error is thrown then we have failed to parse the arguments of your call
#> to `parameter`.  The full prototype of `parameter()` is:
#> 
#>     parameter(default = NULL, constant = NULL, differentiate = FALSE)
#> 
#> We will fail to parse your call if:
#> 
#> • You provide more than three arguments
#> • You provide named arguments that do not match the three above (`default`,
#> `constant` or `differentiate`)
#> 
#> Example:
#> 
#>     x <- parameter(value = 10)
#> 
#> This fails because `value` is not a valid keyword argument to `parameter`.
#> 
```
