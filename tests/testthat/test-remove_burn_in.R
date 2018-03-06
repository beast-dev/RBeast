context("remove_burn_in")

test_that("remove_burn_in: use", {

  # Remove first ten percent
  v <- seq(1, 10)
  w <- remove_burn_in(trace = v, burn_in_fraction = 0.1)
  expect_equal(w, seq(2, 10))
})
