cpp_function <- function(return_type, name, args, body, static = FALSE) {
  c(cpp_args(return_type, name, args, static = static),
    paste0("  ", body),
    "}")
}


cpp_args <- function(return_type, name, args, static = FALSE) {
  static_str <- if (static) "static " else ""
  args_str <- paste(sprintf("%s %s", names(args), unname(args)),
                    collapse = ", ")
  sprintf("%s%s %s(%s) {",
          static_str, return_type, name, args_str)
}


cpp_block <- function(body) {
  c("{",
    paste0("  ", body),
    "}")
}
