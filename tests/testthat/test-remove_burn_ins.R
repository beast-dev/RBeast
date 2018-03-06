context("remove_burn_ins")

test_that("normal use", {

  # Remove first ten percent
  v <- data.frame(x = seq(1, 10), y = seq(11, 20))
  w <- remove_burn_ins(trace = v, burn_in_fraction  = 0.1)
  expected <- data.frame(x = seq(2, 10), y = seq(12, 20))
  names(expected) <- names(w)

  expect_true(all(w == expected))
})
