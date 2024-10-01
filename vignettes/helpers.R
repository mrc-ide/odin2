lang_output <- function(x, lang) {
  cat(c(sprintf("```%s", lang), x, "```"), sep = "\n")
}
cpp_output <- function(x) lang_output(x, "cpp")
r_output <- function(x) lang_output(x, "r")
plain_output <- function(x) lang_output(x, "plain")
