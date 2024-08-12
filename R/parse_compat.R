parse_compat <- function(exprs, action, call) {
  ## Things we can translate:
  ##
  ## user() -> parameter()
  
  ## rbinom() -> Binomial() [etc]
  
  ## dt -> drop
  ## step -> time
  ## t -> time

  ## We also need to cope with the situation where we have three
  ## things to fix in a single expression.
  
  exprs <- lapply(exprs, parse_compat_fix_user, call)
  exprs <- lapply(exprs, parse_compat_fix_distribution, call)
  parse_compat_report(exprs, action, call)
  exprs
}


parse_compat_fix_user <- function(expr, call) {
  is_user_assignment <-
    rlang::is_call(expr$value, c("<-", "=")) &&
    rlang::is_call(expr$value[[3]], "user")
  if (is_user_assignment) {
    res <- match_call(
      expr$value[[3]],
      function(default, integer, min, max, ...) NULL)
    if (!res$success) {
      odin_parse_error(
        "Failed to translate your 'user()' expression to use 'parameter()'",
        "E1016", expr, call = call)
    }
    for (arg in c("integer", "min", "max")) {
      if (!rlang::is_missing(res$value[[arg]])) {
        odin_parse_error(
          c("Can't yet translate 'user()' calls that use the '{arg}' argument",
            i = paste("We don't support this argument in parameter() yet,",
                      "but once we do we will support translation")),
          "E0001", expr, call = call)
      }
    }

    original <- expr$value
    args <- list(as.name("parameter"))
    if (!rlang::is_missing(res$value$default)) {
      args <- c(args, list(res$value$default))
    }
    expr$value[[3]] <- as.call(args)
    expr <- parse_add_compat(expr, "user", original)
  }
  expr
}


parse_compat_fix_distribution <- function(expr, call) {
  ## for most (or all) of these we can do a direct translation, the
  ## args are unnamed.
  translate <- c(
    rbeta = quote(Beta),
    rbinom = quote(Binomial),
    rcauchy = quote(Cauchy),
    rchisq = quote(ChiSquared),
    rexp = quote(Exponential),
    rf = quote(F),
    rgamma = quote(Gamma),
    rgeometric = quote(Geometric),
    rhyper = quote(Hypergeometric),
    rlogis = quote(Logistic),
    rlnorm = quote(Lognormal),
    rnbinom = quote(NegativeBinomial),
    rnorm = quote(Normal),
    rpois = quote(Poisson),
    rt = quote(T),
    runif = quote(Uniform),
    rweibull = quote(Weibull),
    ## Nonstandard interface
    rmultinom = quote(Multinomial),
    rmhyper = quote(MultivariateHypergeometric))

  if (rlang::is_call(expr$value, c("<-", "="))) {
    if (any(names(translate) %in% all.names(expr$value))) {
      original <- expr$value
      expr$value <- substitute_(expr$value, translate)
      expr$compat <- list(type = "distribution", original = original)
    }
  }

  expr
}


parse_add_compat <- function(expr, type, original) {
  if (is.null(expr$compat)) {
    expr$compat <- list(type = type, original = original)
  } else {
    expr$compat$type <- c(expr$compat$type, type)
  }
  expr
}


parse_compat_report <- function(exprs, action, call) {
  i <- !vlapply(exprs, function(x) is.null(x$compat))
  if (action != "silent" && any(i)) {
    description <- c(
      user = "Replace calls to 'user()' with 'parameter()'",
      distribution = paste(
        "Replace calls to r-style random number calls (e.g., 'rnorm()')",
        "with mcstate2-stye calls (e.g., 'Normal()')"))

    type <- lapply(exprs[i], function(x) x$compat$type)
    err <- unlist0(type)

    detail <- NULL
    for (t in intersect(names(description), err)) {
      j <- which(i)[vlapply(type, function(x) t %in% x)]
      ## Getting line numbers here is really hard, so let's just not
      ## try for now and do this on deparsed expressions.
      updated <- vcapply(exprs[j], function(x) deparse1(x$value))
      original <- vcapply(exprs[j], function(x) deparse1(x$compat$original))
      context_t <- set_names(
        c(rbind(updated, original, deparse.level = 0)),
        rep(c("x", "v"), length(updated)))
      detail <- c(detail, description[[t]], cli_nbsp(context_t))
    }

    header <- "Found {length(err)} compatibility issue{?s}"

    if (action == "error") {
      odin_parse_error(c(header, detail), "E1017", exprs[i], call)
    } else {
      cli::cli_warn(c(header, detail), call = call)
    }
  }
}
