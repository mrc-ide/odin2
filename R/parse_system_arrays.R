build_array_table <- function(exprs, call) {
  dims <- list()
  names <- list()
  sizes <- list()
  n <- 1
  for (expr in exprs) {
    names_i <- expr$lhs$names
    dims_i <- expr$rhs$value
    if (rlang::is_call(dims_i, "dim")) {
      size_i <- 1
    } else {
      size_i <- expr_prod(dims_i)
      if (is.null(size_i)) {
        size_i <- list(NULL)
      }
    }

    first_dim <- call("dim", as.symbol(expr$lhs$names[1]))

    for (j in seq_along(names_i)) {
      dims[[n]] <- if (j == 1) dims_i else first_dim
      names[[n]] <- names_i[j]
      sizes[[n]] <- size_i
      n <- n + 1
    }
  }

  data_frame(
    name = unlist(names),
    rank = lengths(dims),
    dims = I(dims),
    size = I(sizes))
}

check_duplicate_dims <- function(arrays, exprs, call) {
  throw_duplicate_dim <- function(name, src) {
    odin_parse_error(
      paste("The variable {name} was given dimensions multiple times."),
      "E2021", src, call)
  }

  names <- unlist(arrays$name)
  if (any(duplicated(names))) {
    dup_dim <- unique(names[duplicated(names)])[1]
    lines <- vlapply(exprs, function(x) {
      isTRUE(x$special == "dim" &
               dup_dim %in% c(x$lhs$names))
    })
    srcs <- lapply(exprs[lines], "[[", "src")
    throw_duplicate_dim(dup_dim, srcs)
  }
}


resolve_array_references <- function(arrays) {
  lookup_array <- function(name, copy_from, d) {
    i <- which(d$name == copy_from)
    if (length(i) == 0) {
      return(NULL)
    }
    dim_i <- d$dims[i]
    if (rlang::is_call(dim_i[[1]], "dim")) {
      rhs_dim_var <- deparse(dim_i[[1]][[2]])
      return(lookup_array(name, rhs_dim_var, d[-i, ]))
    }
    return(list(rank = d$rank[i], alias = d$name[i]))
  }

  arrays$alias <- arrays$name
  is_ref <- vlapply(arrays$dims, rlang::is_call, "dim")

  for (i in which(is_ref)) {
    lhs_dim_var <- arrays$name[i]
    rhs_dim_var <- deparse(arrays$dims[i][[1]][[2]])
    res <- lookup_array(lhs_dim_var, rhs_dim_var, arrays[-i, ])
    if (!is.null(res)) {
      arrays$dims[i] <- list(NULL)
      arrays$size[i] <- NA_integer_
      arrays$rank[i] <- res$rank
      arrays$alias[i] <- res$alias
    }
  }

  arrays
}

resolve_split_dependencies <- function(arrays, call) {
  # Resolve case where
  #   dim(a) <- 1
  #   dim(b, c) <- dim(a)
  # At this point, dim(c) will be aliased to dim(b), not dim(a),
  # so find aliases that actually point to other aliases, and
  # resolve them to something that is not an alias.

  find_non_alias <- function(current, original = current, visited = NULL) {
    stopifnot(!any(duplicated(visited)))

    array <- arrays[arrays$name == current, ]
    if (array$alias == array$name) {
      return(array$alias)
    }
    find_non_alias(array$alias, c(visited, original, array$name))
  }

  not_aliased <- arrays$name[arrays$name == arrays$alias]
  wrong <- arrays$name != arrays$alias & !(arrays$alias %in% not_aliased)
  for (i in which(wrong)) {
    arrays$alias[i] <- find_non_alias(arrays$alias[i], arrays$name[i])
  }

  arrays
}


add_alias_dependency <- function(exprs, arrays) {
  is_alias <- arrays$alias != arrays$name
  if (!any(is_alias)) {
    return(exprs)
  }

  remap <- set_names(
    odin_dim_name(arrays$alias[is_alias]),
    odin_dim_name(arrays$name[is_alias]))

  update_alias_dependency <- function(eq) {
    i <- eq$rhs$depends$variables %in% names(remap)
    if (any(i)) {
      eq$rhs$depends$variables <- union(
        eq$rhs$depends$variables,
        unname(remap[eq$rhs$depends$variables[i]]))
    }
    eq
  }

  lapply(exprs, update_alias_dependency)
}
