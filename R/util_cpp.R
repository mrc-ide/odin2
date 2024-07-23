cpp_function <- function(return_type, name, args, body, static = FALSE) {
  c(cpp_args(return_type, name, args, static = static),
    sprintf("  %s", body),
    "}")
}


cpp_args <- function(return_type, name, args, static = FALSE) {
  static_str <- if (static) "static " else ""
  args_str <- paste(sprintf("%s %s", names(args), unname(args)),
                    collapse = ", ")
  sprintf("%s%s %s(%s) {",
          static_str, return_type, name, args_str)
}
