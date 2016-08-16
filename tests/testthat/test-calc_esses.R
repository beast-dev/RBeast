context("calc_esses")

test_that("calc_esses: use", {

  filename <- system.file(
    "extdata", "beast2_example_output.log", package = "RBeast"
  )

  estimates_raw <- parse_beast_log(
    filename = filename
  )

  # Remove burn-ins
  estimates <- remove_burn_ins(
    estimates_raw,
    burn_in_fraction = 0.1
  )
  df <- calc_esses(estimates, sample_interval = 1000)

  df_expected <- estimates[1, ]
  df_expected[1, ] <- c(3, 10, 10, 10, 10, 7, 10, 9, 6)
  df[1, ] <- as.integer(df[1, ] + 0.5) # Round off


  expect_true(identical(df, df_expected))
})

test_that("calc_esses: abuse", {

  expect_error(
    calc_esses(traces = "not numeric", sample_interval = 1),
    "traces must be a data.frame"
  )

  expect_error(
    calc_esses(
      traces = data.frame(x = seq(1, 10), y = seq(2, 11)),
      sample_interval = 0
    ),
    "sample interval must be at least one"
  )

})
