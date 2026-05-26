js_method <- function(name, args, body) {
  c(
    sprintf("%s(%s) {", name, paste0(args, collapse = ", ")),
    sprintf("  %s", body),
    "},"
  )
}
