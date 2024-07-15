odin_parse_error <- function(msg, src, call, ...,
                             .envir = parent.frame()) {
  if (!is.null(names(src))) {
    src <- list(src)
  }
  cli::cli_abort(msg,
                 class = "odin_parse_error",
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
    src <- unlist(lapply(src, "[[", "str"))
    context <- sprintf("%s| %s", gsub(" ", "\u00a0", format(line)), src)
  }
  c(">" = "Context:", context)
}
