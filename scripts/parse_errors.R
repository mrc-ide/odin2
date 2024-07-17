#!/usr/bin/env Rscript

read_errors <- function() {
  path_rmd <- "vignettes/errors.Rmd"
  txt <- readLines(path_rmd)

  re <- "^# `(E[0-9]{4})`$"
  i <- grep(re, txt)
  if (length(setdiff(grep("^# ", txt), i)) > 0) {
    stop("Some headings don't match expected pattern")
  }

  f <- function(from, to) {
    ret <- txt[from:to]
    while (ret[[1]] == "") {
      ret <- ret[-1]
    }
    while (ret[[length(ret)]] == "") {
      ret <- ret[-length(ret)]
    }
    ret
  }

  ret <- Map(f, i + 1, c(i[-1] - 1, length(txt)))
  names(ret) <- sub(re, "\\1", txt[i])
  ret
}


## We might parse this further, e.g., with commonmark, so that we can
## render this nicely to the console as cli would make this pretty
## easy really.
errors <- read_errors()
save(list = "errors", file = file.path("R/sysdata.rda"), version = 2)
