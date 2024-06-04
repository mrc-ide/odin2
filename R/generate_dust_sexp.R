generate_dust_sexp <- function(expr, dat, options = NULL) {
  if (is.recursive(expr)) {
    fn <- as.character(expr[[1]])
    args <- expr[-1]
    n <- length(args)
    values <- vcapply(args, generate_dust_sexp, dat, options)

    if (fn == "(") {
      ret <- sprintf("(%s)", values[[1]])
    } else if (n == 1 && fn == "-") {
      ret <- sprintf("-%s", values[[1]])
    } else if (n == 1 && fn == "+") {
      ret <- values[[1]]
    } else if (n == 2 && fn %in% c("+", "-", "*", "/")) {
      ret <- sprintf("%s %s %s", values[[1]], fn, values[[2]])
    } else if (fn %in% "exp") {
      ret <- sprintf("mcstate::math::%s(%s)",
                     fn, paste(values, collapse = ", "))
    } else {
      ## TODO: we should catch this elsewhere.
      cli::cli_abort("Unhandled function '{fn}'")
    }
  } else if (is.symbol(expr) || is.character(expr)) {
    name <- as.character(expr)
    if (name %in% c(TIME, DT)) {
      ret <- name
    } else {
      location <- dat$location$location[[name]]
      if (location %in% c("state", "stack")) {
        ret <- name
      } else if (location %in% c("shared", "internal", "data")) {
        if (location == "shared" && isFALSE(options$shared_exists)) {
          ret <- name
        } else {
          ret <- sprintf("%s.%s", location, name)
        }
      } else {
        ## This is a bug!
        cli::cli_abort("Unhandled location '{location}'")
      }
    }
  } else if (is.numeric(expr)) {
    if (expr %% 1 == 0) {
      ret <- format(expr)
    } else {
      ret <- sprintf("static_cast<real_type>(%s)",
                     deparse(expr, control = "digits17"))
    }
  } else if (is.logical(expr)) {
    ret <- tolower(expr)
  } else {
    ## This is a bug!
    cli::cli_abort("Unhandled data type")
  }
  ret
}


generate_dust_sexp_compare <- function(expr, dat) {
  fn <- as.character(expr[[1]])
  args <- expr[-1]
  n <- length(args)
  values <- vcapply(args, generate_dust_sexp, dat)
  sprintf("mcstate::density::%s<real_type>(%s, true)",
          tolower(fn),
          paste(values, collapse = ", "))
}


generate_dust_lhs <- function(name, dat, name_state = "state") {
  location <- dat$location$location[[name]]
  if (location == "stack") {
    sprintf("const %s %s", dat$location$type[[name]], name)
  } else if (location == "state") {
    sprintf("%s[%s]", name_state, dat$location$packing$state[[name]])
  } else {
    stop("Unsupported location")
  }
}
