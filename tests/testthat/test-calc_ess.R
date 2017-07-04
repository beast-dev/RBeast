context("calc_ess")

test_that("calc_ess: use", {

  filename <- system.file(
    "extdata", "beast2_example_output.log", package = "RBeast"
  )

  estimates <- parse_beast_log(
    filename = filename
  )

  esses <- rep(NA, ncol(estimates))
  burn_in_fraction <- 0.1
  for (i in seq_along(estimates)) {
    # Trace with the burn-in still present
    trace_raw <- as.numeric(t(estimates[i]))

    # Trace with the burn-in removed
    trace <- remove_burn_in(trace = trace_raw, burn_in_fraction = 0.1)

    # Store the effectice sample size
    esses[i] <- calc_ess(trace, sample_interval = 1000)
  }

  # Note that the first value of three is nonsense:
  # it is the index of the sample. I keep it in
  # for simplicity of writing this code
  expected_esses <- c(3, 10, 10, 10, 10, 7, 10, 9, 6)
  expect_true(all(expected_esses - esses < 0.5))
})

test_that("calc_ess: abuse", {

  expect_error(
    calc_ess(trace = "not numeric", sample_interval = 1),
    "trace must be numeric"
  )

  expect_error(
    calc_ess(trace = seq(1, 10), sample_interval = 0),
    "sample interval must be at least one"
  )

})
