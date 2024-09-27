##' Migrate odin code.  This function takes a path to existing odin
##' code and writes out migrated code to a new file.  It is possible
##' that no code will be migrated, in which case the written contents
##' will be identical to those read.
##'
##' @title Migrate odin code
##'
##' @param path Path of the odin code to read
##'
##' @param dest Path of the destination code.  It can be the same as
##'   `dest`, in which case the file will be overwritten.
##'
##' @return Nothing; called for side effects only
##'
##' @export
odin_migrate <- function(path, dest) {
  if (!file.exists(path)) {
    cli::cli_abort("File '{path}' does not exist")
  }
  assert_scalar_character(dest)

  txt <- readLines(path)
  call <- environment()
  dat <- parse_prepare(rlang::quo(txt), "text", call)
  exprs <- parse_compat(dat$exprs, "silent", ignore_error = TRUE, call = call)

  is_migratable <- vlapply(exprs, function(x) !is.null(x$compat))
  if (any(is_migratable)) {
    cli::cli_alert_info("Migrating {sum(is_migratable)} statement{?s}")
    ## Note: work backwards in case we change the length
    for (e in rev(exprs[is_migratable])) {
      if (e$end > e$start) {
        j <- seq(e$start, e$end)
        txt[[j[[1]]]] <- deparse1(e$value)
        txt <- txt[-j[-1]]
      } else {
        txt[[e$start]] <- deparse1(e$value)
      }
    }
  } else {
    cli::cli_alert_info("Nothing to migrate")
  }

  is_error <- vlapply(exprs, function(x) !is.null(x$error))
  if (any(is_error)) {
    cli::cli_alert_warning(
      "Skipped {sum(is_error)} statement{?s} that we could not migrate")
    msg <- vcapply(
      exprs[is_error],
      function(x) sprintf("[%s] %s", x$error$code, x$error$message))
    for (m in sort(msg)) {
      cli::cli_alert_danger(m)
      for (line in format_src(exprs[is_error][msg == m])) {
        cli::cli_text(line)
      }
    }
    cli::cli_alert_warning(paste(
      "{cli::qty(sum(is_error))}{?This statement has/These statements have}",
      "been preserved in the migrated code at '{dest}'"))
    cli::cli_alert_info(
      "See {.vignette migrating} for information on updating these issues")
  }

  if (!any(is_migratable) && identical(path, dest)) {
    cli::cli_alert_info("'{path}' is up to date")
  } else {
    writeLines(txt, dest)
    cli::cli_alert_success("Wrote '{dest}'")
  }
}
