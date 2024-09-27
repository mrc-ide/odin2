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
##' * `parameters`: A data.frame describing parameters.  Currently the
##'   only column is `name`.
##' * `variables`: A data.frame describing the model variables.
##'   Currently the only column is `name`.
##' * `data`: A data.frame describing data used by the model (if it
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
##' * `src`: Source information about the error.  This is subject to
##'   change (mrc-5804)
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
##' * `compatibility`: A data.frame of compatibility issues
##'
##' The intention is that we won't throw generally from this function.
##'   However, we *will* throw if your input cannot be found or
##'   classified.  Once we have something that is definitely odin code
##'   we won't throw.
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
odin_validate <- function(expr, input_type = NULL,
                          compatibility = "warning") {
  call <- environment()
  env <- new.env(parent = emptyenv())
  quo <- rlang::enquo(expr)

  res <- rlang::try_fetch(
    odin_parse_quo(quo, input_type, compatibility, call),
    error = identity,
    odin_compatibility_problem = function(e) {
      env$compatibility <- e$data
      tryInvokeRestart("muffleWarning")
    })

  success <- !inherits(res, "error")
  if (success) {
    error <- NULL
    result <- validate_format_result(res)
  } else {
    error <- res
    result <- NULL
  }

  list(success = success,
       error = error,
       result = result,
       compatibility = validate_format_compatibility(env$compatibility))
}


validate_format_result <- function(dat) {
  list(time = dat$time,
       variables = data_frame(name = dat$variables),
       parameters = dat$parameters["name"],
       data = dat$data["name"])
}


validate_format_compatibility <- function(compat) {
  if (is.null(compat)) {
    return(NULL)
  }
  ret <- data_frame(
    index = viapply(compat, "[[", "index"),
    type = vcapply(compat, function(x) x$compat$type),
    description = vcapply(compat, function(x) x$compat$description),
    original = I(lapply(compat, function(x) x$compat$original)),
    value = I(lapply(compat, "[[", "value")))

  if (!is.null(compat[[1]]$str)) {
    ret$src_start <- viapply(compat, "[[", "start")
    ret$src_end <- viapply(compat, "[[", "end")
    ret$src_value <- vcapply(compat, "[[", "str")
  }

  ret
}
