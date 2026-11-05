generate_js_sexp <- function(expr, dat, options = list()) {
  if (is.recursive(expr)) {
    is_stochastic_call <- rlang::is_call(expr[[1]], "OdinStochasticCall")
    if (is_stochastic_call) {
      if (isTRUE(options$stochastic_expectation)) {
        return(generate_js_sexp(expr[[1]]$mean, dat, options))
      }
      fn <- expr[[1]]$sample
    } else {
      fn <- as.character(expr[[1]])
    }

    ## There's a group here where we don't want to evaluate the
    ## arguments, because the interpretation of some values will be
    ## different to odin's normal rewrite semantics.
    if (fn == "[") {
      return(generate_js_array_access(expr, dat, options))
    } else if (fn == "OdinDim") {
      dim <- if (isFALSE(options$shared_exists)) "dim." else "params.dim."
      name <- dat$arrays$alias[match(expr[[2]], dat$arrays$name)]
      return(sprintf("%s%s.dim[%d]", dim, name, expr[[3]] - 1))
    } else if (fn == "OdinLength") {
      dim <- if (isFALSE(options$shared_exists)) "dim." else "params.dim."
      name <- dat$arrays$alias[match(expr[[2]], dat$arrays$name)]
      return(sprintf("%s%s.size", dim, name))
    } else if (fn == "OdinMult") {
      dim <- if (isFALSE(options$shared_exists)) "dim." else "params.dim."
      name <- dat$arrays$alias[match(expr[[2]], dat$arrays$name)]
      return(sprintf("%s%s.mult[%d]", dim, name, expr[[3]] - 1))
    } else if (fn == "OdinOffset") {
      where <- expr[[2]]
      what <- expr[[3]]
      packing <- dat$packing[[where]]
      i <- match(what, packing$name)
      if (is.na(packing$offset[[i]])) {
        shared <- if (isFALSE(options$shared_exists)) "" else "params."
        return(sprintf("%sodin.packing.idx.%s.start", shared, what))
      } else {
        return(as.character(packing$offset[[i]]))
      }
    } else if (fn == "length") {
      return(generate_js_sexp(call("OdinLength", as.character(expr[[2]])),
                                dat, options))
    } else if (fn %in% c("nrow", "ncol")) {
      return(generate_js_sexp(
        call("OdinDim", as.character(expr[[2]]), if (fn == "nrow") 1 else 2),
        dat, options))
    } else if (fn == "OdinReduce") {
      return(generate_js_sexp_reduce(expr, dat, options))
    } else if (fn == "OdinInterpolateAlloc") {
      mode <- expr$mode
      rank <- expr$rank
      substr(mode, 1, 1) <- toupper(substr(mode, 1, 1))
      time_var <- generate_js_sexp(expr$time, dat, options)
      value_var <- generate_js_sexp(expr$value, dat, options)
      stopifnot(mode == "Constant")
      if (rank == 0) {
        return(sprintf(
          'new imports.interpolate.Interpolate%s(%s, %s, "%s", "%s")',
          mode, time_var, value_var, expr$time, expr$value
        ))
      } else {
        dim <- if (isFALSE(options$shared_exists)) "dim." else "params.dim."
        dim_var <- sprintf("%s%s", dim, expr$dim)
        return(sprintf(
          'new imports.interpolate.Interpolate%sArray(%s, %s, %s, "%s", "%s")',
          mode, time_var, value_var, dim_var, expr$time, expr$value
        ))
      }
    } else if (fn == "OdinInterpolateEval") {
      src <- generate_js_sexp(expr[[2]], dat, options)
      target <- expr[[3]]
      return(sprintf("%s.eval(time);", src))
    }

    ## Below here is much simpler, really.
    args_value <- as.list(expr[-1])
    args_str <- vcapply(args_value, generate_js_sexp, dat, options)
    n <- length(args_value)

    binary_compare <- c("==", "!=", "<", ">", "<=", ">=")
    binary_inplace <- c("+", "-", "*", "/", "&&", "||", binary_compare)

    if (fn == "(") {
      ret <- sprintf("(%s)", args_str[[1]])
    } else if (n == 1 && fn == "-") {
      ret <- sprintf("-%s", args_str[[1]])
    } else if (n == 1 && fn == "+") {
      ret <- args_str[[1]]
    } else if (n == 2 && fn %in% binary_inplace) {
      js_fn <- fn
      if (fn %in% c("==", "!=")) {
        ## js equality and non equality operators do type conversion by
        ## default but we don't want this behaviour so convert to strict
        ## versions of those operators
        js_fn <- if (fn == "==") "===" else "!=="
      }
      ret <- sprintf("%s %s %s", args_str[[1]], js_fn, args_str[[2]])
    } else if (fn == "%%") {
      ret <- sprintf("%s %% %s", args_str[[1]], args_str[[2]])
    } else if (fn == "%/%") {
      ret <- sprintf("Math.floor(%s / %s)", args_str[[1]], args_str[[2]])
    } else if (fn %in% c("min", "max")) {
      ret <- sprintf("Math.%s(%s)", fn, paste(args_str, collapse = ", "))
      if (isTRUE(options$array_access)) {
        ## If we are within a `[` expression then we're almost certainly
        ## wanting an integer out
        ret <- sprintf("Math.round(%s)", ret)
      }
    } else if (fn %in% names(FUNCTIONS_JS_MATH)) {
      ret <- sprintf("%s(%s)",
                     FUNCTIONS_JS_MATH[[fn]],
                     paste(args_str, collapse = ", "))
    } else if (fn == "as.numeric") {
      ret <- sprintf("Number(%s)", args_str[[1]])
    } else if (fn == "as.integer") {
      ret <- sprintf("Math.round(%s)", args_str[[1]])
    } else if (fn == "if") {
      ## NOTE: The ternary operator has very low precendence, so we
      ## will agressively parenthesise it.  This is strictly not
      ## needed when this expression is the only element of `expr` but
      ## that's hard to detect so we'll tolerate a few additional
      ## parens for now.
      ret <- sprintf("(%s ? %s : %s)",
                     args_str[[1L]], args_str[[2L]], args_str[[3L]])
    } else if (is_stochastic_call) {
      ## Not all distributions are in random in dust2-js but for now
      ## just letting it error
      stopifnot(fn %in% c("binomial", "uniform", "normal", "poisson"))
      ret <- sprintf("random.%s(%s)",
                     fn, paste(args_str, collapse = ", "))
    } else if (fn == "as.logical") {
      ret <- sprintf("Boolean(%s)", args_str[[1]])
    } else {
      ## TODO: we should catch this during parse; erroring here is a
      ## bug as we don't offer context.
      cli::cli_abort("Unhandled function '{fn}'",
                     class = "odin_bug")
    }
  } else {
    if (is.symbol(expr) || is.character(expr)) {
      name <- as.character(expr)
      if (name %in% c("time", "dt", INDEX)) {
        ret <- name
      } else if (name == "pi") {
        ret <- "Math.PI"
      } else {
        location <- dat$location[[name]]
        shared_exists <- !isFALSE(options$shared_exists)
        if (location %in% c("state", "stack", "adjoint",
                            if (!shared_exists) "shared")) {
          ret <- name
        } else { # params, internal, data
          js_location <- if (location == "shared") "params" else location
          ret <- sprintf("%s.%s", js_location, name)
        }
      }
    } else if (is.numeric(expr)) {
      if (expr == Inf) {
        ret <- "Infinity"
      } else if (expr %% 1 == 0) {
        ret <- format(expr)
      } else {
        ret <- deparse(expr, control = "digits17")
      }
    } else if (is.logical(expr)) {
      ret <- tolower(expr)
    } else {
      cli::cli_abort("Unhandled data type while generating expression",
                     class = "odin_bug")
    }
  }
  ret
}


generate_js_sexp_reduce <- function(expr, dat, options) {
  fn <- expr$fn
  target <- expr$what
  target_str <- generate_js_sexp(target, dat, options)
  index <- expr$index
  dim <- paste0(
    if (isFALSE(options$shared_exists)) "dim." else "params.dim.",
    dat$arrays$alias[match(target, dat$arrays$name)])
  stopifnot(fn %in% c("sum", "prod", "min", "max"))
  if (is.null(index)) {
    sprintf("imports.array.%sAll(%s)", fn, target_str)
  } else {
    index_len <- length(index)
    stopifnot(index_len <= 4)

    index_str <- paste(vcapply(index, function(el) {
      if (el$type == "single") {
        from <- expr_minus(el$at, 1)
        to <- from
      } else {
        from <- expr_minus(el$from, 1)
        to <- expr_minus(el$to, 1)
      }
      sprintf("[%s, %s]",
              generate_js_sexp(from, dat, options),
              generate_js_sexp(to, dat, options))
    }), collapse = ", ")

    sprintf("imports.array.%s%d(%s, %s, %s)",
            fn, index_len, target_str, dim, index_str)
  }
}


generate_js_array_access <- function(expr, dat, options) {
  target <- generate_js_sexp(expr[[2]], dat, options)
  name <- as.character(expr[[2]])
  options$array_access <- TRUE
  idx <- generate_js_sexp(flatten_index(expr[-(1:2)], name), dat, options)
  sprintf("%s[%s]", target, idx)
}
