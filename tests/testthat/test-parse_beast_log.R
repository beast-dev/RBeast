context("parse_beast_log")

test_that("parse_beast_log: use", {

  filename <- system.file(
    "extdata", "beast2_example_output.log", package = "RBeast"
  )

  estimates <- RBeast::parse_beast_log(
    filename = filename
  )
  expected_names <- c(
   "Sample", "posterior", "likelihood",
   "prior", "treeLikelihood", "TreeHeight",
   "BirthDeath", "birthRate2", "relativeDeathRate2"
  )
  testthat::expect_equal(names(estimates), expected_names)

})

test_that("parse_beast_log: abuse", {

  expect_error(
    parse_beast_log(filename = "inva.lid"),
    "file absent"
  )

})
