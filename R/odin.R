##' Compile an odin model, yielding a `dust_system_generator` object.
##'
##' @title Compile an odin model
##'
##' @param expr Odin code as the path to a file (a string), a
##'   character vector of code, or as an expression (typically within
##'   braces `{}`).
##'
##' @param input_type An optional string describing the type of input
##'   for `expr` - must be one of `file`, `text` or `expression`.  If
##'   given, this skips the type detection logic and odin will throw
##'   an error if the wrong type of input is given.  Using this may be
##'   beneficial in programmatic environments.
##'
##' @param compatibility Compatibility mode to use.  Valid options are
##'   "warning", which updates code that can be fixed, with warnings,
##'   and "error", which will error.  The option "silent" will
##'   silently rewrite code, but this is not recommended for general
##'   use as eventually the compatibility mode will be removed (this
##'   option is primarily intended for comparing output of odin1 and
##'   odin2 models against old code).  The default, `NULL`, currently
##'   corresponds to `warning`.
##'
##' @param check_bounds Control over static array bounds checking.
##'   This is enabled by default, but is prone to false positives,
##'   erroring where a read or write appears out of bounds but is
##'   actually ok.  This argument exists to allow you to disable the
##'   check and compile the model anyway.  Future versions may allow
##'   specific lines to be ignored, which will provide finer control
##'   and allow you to use the bits of the checks that are actually
##'   helpful.  You can also pass `TRUE` here to mean "error" or
##'   `FALSE` to mean "disabled".
##'
##' @param ... Unused, must be empty.  This allows us to add and
##'   reorder remaining arguments at will, as all arguments must be
##'   specified by name, not position).
##'
##' @param target Compilation target.  If provided, it must be one of
##'   `cpp` or `js`.  In future, we hope to support `cuda` to enable
##'   support compilation to run on NVIDIA GPUs.  The default, `NULL`,
##'   falls back on the option `odin2.target`, and if that is unset we
##'   **currently** select `cpp`, but this is subject to change once
##'   the JavaScript support is complete.
##'
##' @inheritParams dust2::dust_compile
##'
##' @return A `dust_system_generator` object, suitable for using with
##'   dust functions (starting from [dust2::dust_system_create])
##'
##' @export
##' @examplesIf interactive()
##' # A random walk:
##' gen <- odin({
##'   initial(x) <- 0
##'   update(x) <- Normal(x, 1)
##' })
##'
##' sys <- dust2::dust_system_create(gen, list(), n_particles = 10)
##' y <- dust2::dust_system_simulate(sys, 0:100)
##' matplot(t(y[1, , ]), type = "l", lty = 1, xlab = "Time", ylab = "Value")
odin <- function(expr, ..., input_type = NULL, quiet = NULL, workdir = NULL,
                 debug = NULL, skip_cache = FALSE, compatibility = NULL,
                 check_bounds = NULL, target = NULL) {
  rlang::check_dots_empty()
  call <- environment()
  ## Fail fast here:
  target <- odin_select_target(target)
  dat <- odin_parse_quo(rlang::enquo(expr), input_type, compatibility,
                        check_bounds, call)

  ## This is a bit gross because we only pass some arguments forward.
  ## However, some might end up useful for the js (e.g., quiet,
  ## workdir, skip_cache could all have some potential applications).
  ## Based on feedback though, people prefer explicit names to the
  ## arguments rather than dots and "see some other function for
  ## allowable dots".  We could also tighten up which args are
  ## compatible with which targets, but most of the time it's just not
  ## that big a deal.
  switch(
    target,
    cpp = odin_generator_cpp(dat, quiet = quiet, workdir = workdir,
                             debug = debug, skip_cache = skip_cache),
    js = odin_generator_js(dat),
    cli::cli_abort("Unknown target type '{target}' [odin bug]")) # nocov
}


##' Show generated code from compiling an odin model.
##'
##' @title Show generated odin code
##'
##' @inheritParams odin
##'
##' @param what Optional string, being a single method to show.
##'   Popular options are `update`, `rhs` and `compare_data`.
##'
##' @return A character vector, with class `odin_code` that has a
##'   pretty-print method defined.  Returns `NULL` if `what` was given
##'   but the model lacks this part.
##'
##' @export
##' @examples
##' # Show generated code for the whole system
##' odin_show({
##'   initial(x) <- 1
##'   update(x) <- a
##'   a <- Normal(x, 1)
##' })
##'
##' # Just the update method
##' odin_show({
##'   initial(x) <- 1
##'   update(x) <- a
##'   a <- Normal(x, 1)
##' }, what = "update")
odin_show <- function(expr, input_type = NULL, compatibility = NULL,
                      check_bounds = NULL, what = NULL) {
  call <- environment()
  dat <- odin_parse_quo(rlang::enquo(expr), input_type, compatibility,
                        check_bounds, call)
  if (!is.null(what)) {
    parts <- generate_dust_parts()
    dat <- generate_prepare(dat)
    code <- parts[[match_value(what, names(parts))]](dat)
    attr(code, "what") <- what
  } else {
    code <- generate_dust_system(dat)
  }
  if (!is.null(code)) {
    class(code) <- "odin_code"
  }
  code
}


##' @export
print.odin_code <- function(x, ...) {
  ## We can probably do more with this later, but this should already
  ## be enough:
  what <- attr(x, "what")
  if (is.null(what)) {
    cli::cli_h1("odin code:")
  } else {
    cli::cli_h1("odin code ({what}):")
  }
  writeLines(x)
  invisible(x)
}


odin_select_target <- function(target, call = parent.frame()) {
  if (is.null(target)) {
    target <- getOption("odin2.target", "cpp")
  }
  valid <- c("cpp", "js")
  match_value(target, valid, call = call)
}


odin_generator_cpp <- function(dat, quiet, workdir, debug, skip_cache) {
  code <- generate_dust_system(dat)
  tmp <- tempfile(fileext = ".cpp")
  on.exit(unlink(tmp))
  writeLines(code, tmp)
  dust2::dust_compile(tmp, quiet = quiet, workdir = workdir, debug = debug,
                      skip_cache = skip_cache)
}


odin_generator_js <- function(dat) {
  ## Generate JS code, create a V8 context that contains the required
  ## support code, evaluate our JS code in that context and return a
  ## wrapper object that contains the context and matches the
  ## interface of the dust/C++ version.  That suggests that some of
  ## the work here really ends up being pushed into dust because we
  ## want dust_system_create to behave appropriately based on the
  ## first argument -- which would here be a js generator.  That means
  ## that the dust package probably needs to report back about
  ## supported backends (e.g., is V8 installed vs is a C++ toolchain
  ## installed) and smooth over as much of this as possible.
  cli::cli_abort(
    c("JavaScript is not yet supported",
      i = 'You have used `target = "js"` before we have implemented it'),
    call = parent.frame())
}
