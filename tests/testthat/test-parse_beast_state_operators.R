context("parse_beast_state_operators")

test_that("column names are correct", {

  xml_state_filename <- system.file(
    "extdata", "beast2_example_output.xml.state", package = "RBeast"
  )
  estimates <- parse_beast_state_operators(filename = xml_state_filename)
  expected_names <- c("operator", "p", "accept", "reject", "acceptFC",
    "rejectFC", "rejectIv", "rejectOp")
  testthat::expect_equal(names(estimates), expected_names)
})
