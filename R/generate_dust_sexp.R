generate_dust_sexp <- function(expr, dat) {
  if (is.recursive(expr)) {
    fn <- as.character(expr[[1]])
    args <- expr[-1]
    n <- length(args)
    values <- vcapply(args, generate_dust_sexp, dat)

    if (fn == "(") {
      ret <- sprintf("(%s)", values[[1]])
    } else if (n == 1 && fn == "-") {
      ret <- sprintf("-%s", values[[1]])
    } else if (n == 1 && fn == "+") {
      ret <- values[[1]]
    } else if (n == 2 && fn %in% c("+", "-", "*", "/")) {
      ret <- sprintf("%s %s %s", values[[1]], fn, values[[2]])
    } else {
      ## TODO: we should catch this elsewhere.
      stop("Unhandled function")
    }
  } else if (is.symbol(expr)) {
    name <- as.character(expr)
    location <- dat$location$location[[name]]
    if (location %in% c("state", "stack")) {
      ret <- name
    } else if (location %in% c("shared", "internal", "data")) {
      ret <- sprintf("%s.%s", location, name)
    } else {
      stop("Unhandled location")
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
    stop("Unhandled data type")
  }
  ret
}


generate_dust_sexp_compare <- function(expr, dat) {
  fn <- as.character(expr[[1]])
  args <- expr[-1]
  n <- length(args)
  values <- vcapply(args, generate_dust_sexp, dat)
  sprintf("mcstate2::density::%s(%s, true)",
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
