odin_parse_error <- function(msg, code, src, call, ...,
                             .envir = parent.frame()) {
  stopifnot(grepl("^E[0-9]{4}$", code))
  if (!is.null(names(src))) {
    src <- list(src)
  }
  cli::cli_abort(msg,
                 class = "odin_parse_error",
                 code = code,
                 src = src,
                 call = call,
                 ...,
                 .envir = .envir)
}


##' @importFrom rlang cnd_footer
##' @export
cnd_footer.odin_parse_error <- function(cnd, ...) {
  ## TODO: later, we might want to point at specific bits of the error
  ## and say "here, this is where you are wrong" but that's not done
  ## yet...
  src <- cnd$src

  if (is.null(src[[1]]$str)) {
    context <- unlist(lapply(cnd$src, function(x) deparse1(x$value)))
  } else {
    line <- unlist(lapply(src, function(x) seq(x$start, x$end)))
    src_str <- unlist(lapply(src, "[[", "str"))
    context <- sprintf("%s| %s", cli_nbsp(format(line)), src_str)
  }

  code <- cnd$code
  ## See https://cli.r-lib.org/reference/links.html#click-to-run-code
  ## RStudio will only run code in namespaced form
  explain <- cli::format_inline(
    "For more information, run {.run odin2::odin_error_explain(\"{code}\")}")

  ## It's quite annoying to try and show the original and updated code
  ## here at the same time so instead let's just let the user know
  ## that things might not be totally accurate.
  uses_compat <- !vlapply(src, function(x) is.null(x$compat))
  if (any(uses_compat)) {
    compat_warning <- c(
      "!" = cli::format_inline(
        paste("{cli::qty(length(src))}{?The expression/Expressions} above",
              "{?has/have} been translated while updating for use with",
              "odin2, the context may not reflect your original code.")))
  } else {
    compat_warning <- NULL
  }

  c(">" = "Context:", context,
    "i" = explain,
    compat_warning)
}


##' Explain error codes produced by odin.  This is a work in progress,
##' and we would like feedback on what is useful as we improve it.
##' The idea is that if you see an error you can link through to get
##' more information on what it means and how to resolve it.  The
##' current implementation of this will send you to the rendered
##' vignettes, but in future we will arrange for offline rendering
##' too.
##'
##' @title Explain odin error
##'
##' @param code The error code, as a string, in the form `Exxxx` (a
##'   capital "E" followed by four numbers)
##'
##' @return Nothing, this is called for its side effect only
##'
##' @export
odin_error_explain <- function(code) {
  ## There are a couple of options here that we can explore:

  ## * We might write out html and read that in as html
  ## * We might render the markdown using cli
  ## * We might page the file with page()
  ## * We might send the user to the web page version

  assert_scalar_character(code)
  if (!grepl("^E[0-9]{4}$", code)) {
    cli::cli_abort("Invalid code '{code}', should match 'Exxxx'",
                   arg = "code")
  }
  txt <- errors[[code]]
  if (is.null(txt)) {
    cli::cli_abort(
      c("Error '{code}' is undocumented",
        i = paste("If you were directed here from an error message, please",
                  "let us know (e.g., file an issue or send us a message)")),
      arg = "code")
  }
  url <- sprintf("https://mrc-ide.github.io/odin2/articles/errors.html#%s",
                 tolower(code))
  utils::browseURL(url)
}
