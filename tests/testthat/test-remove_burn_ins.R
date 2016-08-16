context("remove_burn_ins")

test_that("remove_burn_ins: use", {

  # Remove first ten percent
  v <- data.frame(x = seq(1, 10), y = seq(11, 20))
  w <- remove_burn_ins(trace = v, burn_in_fraction  = 0.1)
  expected <- data.frame(x = seq(2, 10), y = seq(12, 20))
  names(expected) <- names(w)

  expect_true(all(w == expected))
})


test_that("remove_burn_ins: abuse", {

  v <- data.frame(x = seq(1, 10), y = seq(11, 20))

  expect_error(
    remove_burn_ins(traces = v, burn_in_fraction  = -0.1),
    "burn_in_fraction must be at least zero"
  )

  expect_error(
    remove_burn_ins(traces = v, burn_in_fraction  = 1.1),
    "burn_in_fraction must be at most one"
  )

  expect_error(
    remove_burn_ins(traces = "not a valid trace", burn_in_fraction  = 0.1),
    "traces must be a data.frame"
  )

})
