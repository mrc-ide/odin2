generate_dust_sexp <- function(expr, dat, options = list()) {
  if (is.recursive(expr)) {
    is_stochastic_call <- rlang::is_call(expr[[1]], "OdinStochasticCall")
    if (is_stochastic_call) {
      fn <- expr[[1]]$sample
    } else {
      fn <- as.character(expr[[1]])
    }

    if (fn == "[") {
      return(generate_dust_array_access(expr, dat, options))
    } else if (fn == "OdinDim") {
      dim <- if (isFALSE(options$shared_exists)) "dim_" else "shared.dim."
      return(sprintf("%s%s.dim[%d]", dim, expr[[2]], expr[[3]] - 1))
    } else if (fn == "OdinLength") {
      dim <- if (isFALSE(options$shared_exists)) "dim_" else "shared.dim."
      return(sprintf("%s%s.size", dim, expr[[2]]))
    } else if (fn == "OdinOffset") {
      where <- expr[[2]]
      what <- expr[[3]]
      packing <- dat$packing[[where]]
      i <- match(what, packing$name)
      if (is.numeric(packing$offset[[i]])) {
        return(as.character(packing$offset[[i]]))
      } else {
        shared <- if (isFALSE(options$shared_exists)) "" else "shared."
        return(sprintf("%soffset.%s.%s", shared, where, what))
      }
    }

    args <- vcapply(expr[-1], generate_dust_sexp, dat, options)
    n <- length(args)

    if (fn == "(") {
      ret <- sprintf("(%s)", args[[1]])
    } else if (n == 1 && fn == "-") {
      ret <- sprintf("-%s", args[[1]])
    } else if (n == 1 && fn == "+") {
      ret <- args[[1]]
    } else if (n == 2 && fn %in% c("+", "-", "*", "/", "==")) {
      ## Some care might be needed for division in some cases.
      ret <- sprintf("%s %s %s", args[[1]], fn, args[[2]])
    } else if (fn == "%%") {
      ## TODO: we'll use our usual fmodr thing here once we get that
      ## into monty's math library, but for now this is ok.
      ret <- sprintf("std::fmod(%s, %s)", args[[1]], args[[2]])
    } else if (fn %in% names(FUNCTIONS_MONTY_MATH)) {
      ret <- sprintf("monty::math::%s(%s)",
                     FUNCTIONS_MONTY_MATH[[fn]],
                     paste(args, collapse = ", "))
    } else if (fn == "as.numeric") {
      ret <- sprintf("static_cast<real_type>(%s)", args[[1]])
    } else if (fn == "as.integer") {
      ret <- sprintf("static_cast<int>(%s)", args[[1]])
    } else if (fn %in% c("min", "max")) {
      is_integer_like <- grepl("^[0-9]$", args)
      if (any(is_integer_like) && !all(is_integer_like)) {
        args[is_integer_like] <- sprintf("static_cast<real_type>(%s)",
                                         args[is_integer_like])
      }
      ret <- sprintf("std::%s(%s)", fn, paste(args, collapse = ", "))
    } else if (fn == "if") {
      ## NOTE: The ternary operator has very low precendence, so we
      ## will agressively parenthesise it.  This is strictly not
      ## needed when this expression is the only element of `expr` but
      ## that's hard to detect so we'll tolerate a few additional
      ## parens for now.
      ret <- sprintf("(%s ? %s : %s)", args[[1L]], args[[2L]], args[[3L]])
    } else if (is_stochastic_call) {
      ret <- sprintf("monty::random::%s<real_type>(rng_state, %s)",
                     fn, paste(args, collapse = ", "))
    } else {
      ## TODO: we should catch this during parse; erroring here is a
      ## bug as we don't offer context.
      cli::cli_abort("Unhandled function '{fn}'",
                     class = "odin_bug")
    }
  } else if (is.symbol(expr) || is.character(expr)) {
    name <- as.character(expr)
    if (name %in% c("time", "dt", INDEX)) {
      ret <- name
    } else {
      location <- dat$location[[name]]
      shared_exists <- !isFALSE(options$shared_exists)
      if (location %in% c("state", "stack", "adjoint",
                          if (!shared_exists) "shared")) {
        ret <- name
      } else { # shared, internal, data
        ret <- sprintf("%s.%s", location, name)
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
    cli::cli_abort("Unhandled data type while generating expression",
                   class = "odin_bug")
  }
  ret
}


## We'll make this easier to use over time for pulling together the
## intermediate bits of data that we need for generating things.  For
## now all we need is information on where things are to be found (the
## location) but we'll need to cope with variable packing, array
## lengths and types soon.
generate_dust_dat <- function(location, packing) {
  list(location = location, packing = packing)
}


generate_dust_array_access <- function(expr, dat, options) {
  if (length(expr) != 3) {
    stop("need more to generate multidimensional access")
  }
  target <- generate_dust_sexp(expr[[2]], dat, options)
  idx <- generate_dust_sexp(expr_minus(expr[[3]], 1), dat, options)
  sprintf("%s[%s]", target, idx)
}
