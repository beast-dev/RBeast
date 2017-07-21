context("calc_ess")

test_that("exanple usage", {

  filename <- system.file(
    "extdata", "beast2_example_output.log", package = "RBeast"
  )

  # Parse the file as-is and conclude the sampling interval
  df <- RBeast::parse_beast_log(
    filename = filename
  )
  sample_interval <- df$Sample[2] - df$Sample[1]

  # Only keep the parameter estimates, do not care about the sampling times anymore
  estimates <- subset(df, select = -Sample)

  esses <- rep(NA, ncol(estimates))
  burn_in_fraction <- 0.1
  for (i in seq_along(estimates)) {
    # Trace with the burn-in still present
    trace_raw <- as.numeric(t(estimates[i]))

    # Trace with the burn-in removed
    trace <- RBeast::remove_burn_in(trace = trace_raw, burn_in_fraction = 0.1)

    # Store the effectice sample size
    esses[i] <- RBeast::calc_ess(trace, sample_interval = sample_interval)
  }

  # Use the values that TRACER shows
  expected_esses <- c(10, 10, 10, 10, 7, 10, 9, 6)
  testthat::expect_true(all(expected_esses - esses < 0.5))
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
