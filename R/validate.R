##' Validate odin code.  This is primarily intended for use within
##' other applications.
##'
##' # Result
##'
##' On successful validation, we return a list with metadata about the
##' model.  Currently this contains:
##'
##' * `time`: The time mode of the model (a string of either
##'   "discrete" or "continuous")
##' * `parameters`: A `data.frame` describing parameters.  Currently the
##'   only column is `name`.
##' * `variables`: A `data.frame` describing the model variables.
##'   Currently the only column is `name`.
##' * `data`: A `data.frame` describing data used by the model (if it
##'   supports this).  Currently the only column is `name`.
##'
##' # Errors
##'
##' Most errors will have class `odin_parse_error`.  These will print
##' context information if rethrown.  They have fields:
##'
##' * `message`: The headline error message
##' * `code`: The odin error code, as listed in `vignette("errors")`,
##'   and used by [odin_error_explain]
##' * `src`: Source information about the error.  This is a
##'   `data.frame` with columns `index` (the expression number),
##'   `expr` (a list column with the expression), `start` (the
##'   starting line; possibly `NA`), `end` (the finishing line;
##'   possibly `NA`), `str` (the string containing the literal value
##'   of the expression; possibly `NA`) and `migrated` (a logical,
##'   indicating if the source has been automatically migrated from
##'   odin1 code).  If any of `start`, `end` or `str` is `NA`, all
##'   will be, for all rows.
##'
##' You can get the full rendered message using [conditionMessage()]
##' on the error object.
##'
##' @title Validate odin code
##'
##' @inheritParams odin
##'
##' @return A list with elements:
##'
##' * `success`: boolean, `TRUE` if validation was successful
##' * `result`: Metadata about the model; see Details above for the
##'   format.
##' * `error`: Either `NULL` (if `success` is `TRUE`) or an error;
##'   see Details above for interpreting this value.
##' * `compatibility`: A `data.frame` of compatibility issues.  This
##'   is formatted similarly to `src` within `error` (see above), but
##'   also includes `type` (a code for the compatibility issue),
##'   `description` (a human-readable description of the issue),
##'   `original` (the original expression) and `value` (the final
##'   expression).
##'
##' The intention is that we won't throw generally from this function.
##'
##' @export
##' @examples
##' # A successful validation:
##' odin_validate({
##'   initial(x) <- 1
##'   deriv(x) <- a
##'   a <- parameter()
##' })
##'
##' # A failure:
##' odin_validate({
##'   initial(x) <- 1
##'   deriv(x) <- a
##' })
##'
##' # Migration warnings
##' odin_validate({
##'   initial(x)
##'   deriv(x) <- a
##'   a <- user()
##' })
odin_validate <- function(expr, input_type = NULL, compatibility = NULL,
                          check_bounds = NULL) {
  call <- environment()
  env <- new.env(parent = emptyenv())
  quo <- rlang::enquo(expr)

  res <- rlang::try_fetch(
    odin_parse_quo(quo, input_type, compatibility, check_bounds, call),
    error = identity,
    odin_compatibility_problem = function(e) {
      env$compatibility <- e$data
      tryInvokeRestart("muffleWarning")
    })

  success <- !inherits(res, "error")
  if (success) {
    error <- NULL
    result <- list(time = res$time,
                   variables = data_frame(name = res$variables),
                   parameters = res$parameters["name"],
                   data = res$data["name"])
  } else {
    error <- res
    result <- NULL
  }

  list(success = success,
       error = error,
       result = result,
       compatibility = env$compatibility)
}
