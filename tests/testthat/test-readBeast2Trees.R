context("readBeast2Trees")

test_that("readBeast2Trees: use", {
  trees_file <- "../../inst/extdata/readBeast2TreesExample.trees"
  testit::assert(file.exists(trees_file))
  posterior <- readBeast2Trees(trees_file)
  expect_equal(length(posterior), 10)
  expect_equal(class(posterior[[1]]), "phylo")
})

