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
  src <- cnd$src[order(viapply(cnd$src, function(x) x$index %||% 1L))]

  if (is.null(src[[1]]$str)) {
    context <- unlist(lapply(src, function(x) deparse1(x$value)))
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
##' @param how How to explain the error. Options are `pretty` (render
##'   pretty text in the console), `plain` (display plain text in the
##'   console) and `link` (browse to the online help).
##'
##' @return Nothing, this is called for its side effect only
##'
##' @export
odin_error_explain <- function(code, how = "pretty") {
  assert_scalar_character(code)
  if (!grepl("^E[0-9]{4}$", code)) {
    cli::cli_abort("Invalid code '{code}', should match 'Exxxx'",
                   arg = "code")
  }
  err <- errors[[code]]
  if (is.null(err)) {
    cli::cli_abort(
      c("Error '{code}' is undocumented",
        i = paste("If you were directed here from an error message, please",
                  "let us know (e.g., file an issue or send us a message)")),
      arg = "code")
  }

  how <- match_value(how, c("pretty", "plain", "link"))
  if (how == "link") {
    url <- sprintf("https://mrc-ide.github.io/odin2/articles/errors.html#%s",
                   tolower(code))
    utils::browseURL(url)
  } else {
    render_error(err, how == "pretty")
  }
}


## This is probably destined for the utils package once we're happy
## with it.
render_error <- function(err, pretty) {
  render_paragraph <- function(el) {
    cli::cli_par()
    cli::cli_text(el$text)
  }

  render_code_block <- function(el) {
    cli::cli_par()
    cli::cli_code(paste0("    ", el$text))
  }

  render_list <- function(el) {
    cli::cli_par()
    if (el$mode == "bullet") {
      cli::cli_ul()
    } else {
      cli::cli_ol()
    }
    for (i in el$items) {
      cli::cli_li(i$text)
    }
  }

  if (pretty) {
    cli::cli_h1(err$code)
    for (el in err$parsed) {
      switch(
        el$type,
        paragraph = render_paragraph(el),
        code_block = render_code_block(el),
        list = render_list(el),
        cli::cli_abort("Unknown error block type '{el$type}' while rendering"))
    }
  } else {
    writeLines(c(sprintf("# %s", err$code), "", err$plain))
  }
}
