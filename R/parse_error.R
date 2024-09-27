odin_parse_error <- function(msg, code, src, call, ...,
                             .envir = parent.frame()) {
  stopifnot(grepl("^E[0-9]{4}$", code))
  cli::cli_abort(msg,
                 class = "odin_parse_error",
                 code = code,
                 src = parse_error_src(src),
                 call = call,
                 ...,
                 .envir = .envir)
}


parse_error_src <- function(src) {
  if (is.null(src)) {
    src <- list()
  } else if (!is.null(names(src))) {
    src <- list(src)
  }

  ## For now:
  stopifnot(is.list(src))
  stopifnot(!any(vlapply(src, function(x) is.null(x$value))))
  ret <- data_frame(
    index = viapply(src, "[[", "index"),
    expr = I(lapply(src, "[[", "value")),
    start = viapply(src, function(x) x$start %||% NA_integer_),
    end = viapply(src, function(x) x$end %||% NA_integer_),
    str = vcapply(src, function(x) x$str %||% NA_character_),
    migrated = vlapply(src, function(x) !is.null(x$compat)))
  if (nrow(ret) > 0) {
    ret <- ret[order(ret$index), ]
    rownames(ret) <- NULL
  }
  ret
}


##' @importFrom rlang cnd_footer
##' @export
cnd_footer.odin_parse_error <- function(cnd, ...) {
  ## TODO: later, we might want to point at specific bits of the error
  ## and say "here, this is where you are wrong" but that's not done
  ## yet...
  context <- format_src(cnd$src)

  code <- cnd$code
  ## See https://cli.r-lib.org/reference/links.html#click-to-run-code
  ## RStudio will only run code in namespaced form
  explain <- cli::format_inline(
    "For more information, run {.run odin2::odin_error_explain(\"{code}\")}")

  ## It's quite annoying to try and show the original and updated code
  ## here at the same time so instead let's just let the user know
  ## that things might not be totally accurate.
  if (any(cnd$src$migrated)) {
    compat_warning <- c(
      "!" = cli::format_inline(
        paste("{cli::qty(nrow(cnd$src))}{?The expression/Expressions} above",
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
  error_explain(errors, code, how)
}


format_src <- function(src) {
  if (anyNA(src$str)) {
    ## TODO: why not vcapply here? or deparse rather than deparse1 if
    ## using unlist?
    context <- unlist0(lapply(src$expr, deparse1))
  } else {
    ## TODO: check we cope here with newlines and comments, especially
    ## mixed.  We might need to get the original source back in?
    line <- unlist(Map(seq, src$start, src$end))
    src_str <- unlist(strsplit(src$str, "\n"))
    context <- cli_nbsp(sprintf("%s %s",
                                cli::col_grey(paste0(format(line)), "|"),
                                cli::code_highlight(src_str)))
  }
  context
}
