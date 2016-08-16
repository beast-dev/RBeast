context("is_posterior")

test_that("is_posterior: use", {

  trees_filename <- system.file(
    "extdata", "beast2_example_output.trees", package = "RBeast"
  )
  testit::assert(file.exists(trees_filename))

  log_filename <- system.file(
    "extdata", "beast2_example_output.log", package = "RBeast"
  )
  testit::assert(file.exists(log_filename))

  posterior <- parse_beast_posterior(
    trees_filename = trees_filename,
    log_filename = log_filename
  )
  expect_true(is_posterior(posterior))

})
