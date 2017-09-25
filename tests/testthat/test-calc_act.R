context("calc_act")

test_that("calc_act use", {

  trace <- sin(seq(from = 0.0, to = 2.0 * pi, length.out = 100))
  act <- RBeast::calc_act(
    trace = trace,
    sample_interval = 1
  )
  testthat::expect_equal(object = act, expected = 38.18202, tolerance = 0.01)
})

test_that("calc_act_r use", {

  trace <- sin(seq(from = 0.0, to = 2.0 * pi, length.out = 100))
  act <- RBeast::calc_act_r(
    trace = trace,
    sample_interval = 1
  )
  testthat::expect_equal(object = act, expected = 38.18202, tolerance = 0.01)
})


test_that("calc_act abuse", {

  expect_error(
    calc_act(trace = "not numeric", sample_interval = 1),
    "trace must be numeric"
  )

  expect_error(
    calc_act(trace = seq(1, 10), sample_interval = 0),
    "sample interval must be at least one"
  )

})
