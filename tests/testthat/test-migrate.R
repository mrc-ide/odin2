test_that("can migrate code", {
  tmp <- withr::local_tempdir()
  writeLines(
    c("initial(x)<-1",
      "update(x)<-a",
      "a<-user()"),
    file.path(tmp, "original.R"))
  res <- evaluate_promise(
    withr::with_dir(tmp, odin_migrate("original.R", "migrated.R")))
  expect_length(res$messages, 2)
  expect_match(res$messages[[1]], "Migrating 1 statement", fixed = TRUE)
  expect_match(res$messages[[2]], "Wrote 'migrated.R'", fixed = TRUE)

  expect_equal(
    readLines(file.path(tmp, "migrated.R")),
    c("initial(x)<-1",
      "update(x)<-a",
      "a <- parameter()"))
})


test_that("Don't write up-to-date files if in place", {
  tmp <- withr::local_tempdir()
  src <- c("initial(x)<-1",
           "update(x)<-a",
           "a<-parameter()")
  writeLines(src, file.path(tmp, "path.R"))
  res <- evaluate_promise(
    withr::with_dir(tmp, odin_migrate("path.R", "path.R")))
  expect_length(res$messages, 2)
  expect_match(res$messages[[1]], "Nothing to migrate", fixed = TRUE)
  expect_match(res$messages[[2]], "'path.R' is up to date", fixed = TRUE)
  expect_equal(readLines(file.path(tmp, "path.R")), src)
})


test_that("Do write up-to-date files if in new path", {
  tmp <- withr::local_tempdir()
  src <- c("initial(x)<-1",
           "update(x)<-a",
           "a<-parameter()")
  writeLines(src, file.path(tmp, "original.R"))
  res <- evaluate_promise(
    withr::with_dir(tmp, odin_migrate("original.R", "migrated.R")))
  expect_length(res$messages, 2)
  expect_match(res$messages[[1]], "Nothing to migrate", fixed = TRUE)
  expect_match(res$messages[[2]], "Wrote 'migrated.R'", fixed = TRUE)
  expect_equal(readLines(file.path(tmp, "migrated.R")), src)
})


test_that("error if input file is not found", {
  tmp <- withr::local_tempdir()
  expect_error(
    withr::with_dir(tmp, odin_migrate("original.R", "migrated.R")),
    "File 'original.R' does not exist")
})


test_that("Write what we can, even if there are errors", {
  tmp <- withr::local_tempdir()
  src <- c("initial(x)<-0",
           "update(x)<- a + step",
           "a<-user()")
  writeLines(src, file.path(tmp, "original.R"))
  res <- evaluate_promise(
    withr::with_dir(tmp, odin_migrate("original.R", "migrated.R")))
  expect_equal(
    readLines(file.path(tmp, "migrated.R")),
    c("initial(x)<-0",
      "update(x)<- a + step",
      "a <- parameter()"))

  expect_length(res$messages, 7)
  expect_match(res$messages[[1]], "Migrating 1 statement")
  expect_match(res$messages[[2]], "Skipped 1 statement")
  expect_match(res$messages[[3]], "Use of 'step' is no longer allowed")
  expect_match(res$messages[[4]], "update(x)<- a + step", fixed = TRUE)
  expect_match(res$messages[[5]], "This statement has been preserved")
})


test_that("Safely migrate a multiline statement", {
  tmp <- withr::local_tempdir()
  writeLines(
    c("initial(x)<-b",
      "a<-user(",
      "  4,",
      "  integer = TRUE",
      "  )",
      "update(x)<-a",
      "b <- user()",
      "c <- 1"),
    file.path(tmp, "original.R"))
  res <- evaluate_promise(
    withr::with_dir(tmp, odin_migrate("original.R", "migrated.R")))
  expect_equal(
    readLines(file.path(tmp, "migrated.R")),
    c("initial(x)<-b",
      'a <- parameter(4, type = "integer")',
      "update(x)<-a",
      "b <- parameter()",
      "c <- 1"))
})
