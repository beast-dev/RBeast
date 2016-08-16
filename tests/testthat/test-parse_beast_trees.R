context("parse_beast_trees")

test_that("parse_beast_trees: use", {

  filename <- system.file(
    "extdata", "beast2_example_output.trees", package = "RBeast"
  )

  posterior <- parse_beast_trees(
    filename = filename
  )
  expect_true(is_trees_posterior(posterior))
})

test_that("parse_beast_trees: abuse", {

  expect_error(
    parse_beast_trees(filename = "inva.lid"),
    "file absent"
  )

  # To be fixed, see
  # https://github.com/richelbilderbeek/Cer2016/issues/118
  if (1 == 2) {
    log_filename <- system.file(
     "extdata", "beast2_example_output.log", package = "RBeast"
    )
    expect_error(
      parse_beast_trees(
        filename = log_filename
      ),
      "argument of length 0"
    )
  }

})
