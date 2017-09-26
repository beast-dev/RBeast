context("is_posterior")

test_that("detect posterior", {

  trees_filename <- system.file(
    "extdata", "beast2_example_output.trees", package = "RBeast"
  )
  testit::assert(file.exists(trees_filename))

  log_filename <- system.file(
    "extdata", "beast2_example_output.log", package = "RBeast"
  )
  testit::assert(file.exists(log_filename))

  posterior <- RBeast::parse_beast_posterior(
    trees_filename = trees_filename,
    log_filename = log_filename
  )
  testthat::expect_true(is_posterior(posterior))

})

test_that("detect non-posteriors", {

  testthat::expect_false(
    RBeast::is_posterior("nonsense")
  )

})
