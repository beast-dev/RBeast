context("is_trees_posterior")

test_that("can find is_trees_posterior_test.R", {
  filename <- system.file(
    "extdata", "beast2_example_output.trees", package = "RBeast"
  )
  file_exists <- file.exists(filename)
  expect_true(file_exists)
})

test_that("can create a posterior", {
  filename <- system.file(
    "extdata", "beast2_example_output.trees", package = "RBeast"
  )
  posterior <- parse_beast_trees(
    filename
  )
  expect_true(is_trees_posterior(posterior))
})
