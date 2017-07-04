context("remove_burn_in")

test_that("remove_burn_in: use", {

  # Remove first ten percent
  v <- seq(1, 10)
  w <- remove_burn_in(trace = v, burn_in_fraction = 0.1)
  expect_equal(w, seq(2, 10))

  # Remove none of a thousand
  v <- seq(1, 1000)
  w <- remove_burn_in(trace = v, burn_in_fraction = 0.0)
  expect_equal(v, w)

  # Remove all a thousand
  v <- seq(1, 1000)
  w <- remove_burn_in(trace = v, burn_in_fraction = 1.0)
  expect_equal(length(w), 0)

})


test_that("remove_burn_in: abuse", {

  v <- seq(1, 10)

  expect_error(
    remove_burn_in(trace = v, burn_in_fraction = -0.1),
    "burn_in_fraction must be at least zero"
  )

  expect_error(
    remove_burn_in(trace = v, burn_in_fraction = 1.1),
    "burn_in_fraction must be at most one"
  )

  expect_error(
    remove_burn_in(trace = "not a valid trace", burn_in_fraction = 0.1),
    "trace must be numeric"
  )

})
