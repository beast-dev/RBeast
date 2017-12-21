#' Calculate the auto-correlation time using only R. Consider using calc_act
#' instead, as it is orders of magnitude faster
#' @param trace the values
#' @param sample_interval the interval in timesteps between samples
#' @return the auto_correlation time
#' @examples
#'   trace <- sin(seq(from = 0.0, to = 2.0 * pi, length.out = 100))
#'   act <- RBeast::calc_act_r(
#'     trace = trace,
#'     sample_interval = 1
#'   )
#'   testthat::expect_equal(object = act, expected = 38.18202, tolerance = 0.01)
#' @export
#' @seealso Java code can be found here: \url{https://github.com/CompEvol/beast2/blob/9f040ed0357c4b946ea276a481a4c654ad4fff36/src/beast/core/util/ESS.java#L161}
#' @author The original Java version of the algorithm was from Remco Bouckaert,
#'   ported to R and adapted by Richel J.C. Bilderbeek
calc_act_r <- function(trace, sample_interval) {
  beastier::calc_act_r(trace, sample_interval)
}

#' Calculate the auto-correlation time, alternative implementation
#' @param trace the values
#' @param sample_interval the interval in timesteps between samples
#' @return the auto_correlation time
#' @export
#' @examples
#'   trace <- sin(seq(from = 0.0, to = 2.0 * pi, length.out = 100))
#'   act <- RBeast::calc_act(
#'     trace = trace,
#'     sample_interval = 1
#'   )
#'   testthat::expect_equal(object = act, expected = 38.18202, tolerance = 0.01)
#' @seealso Java code can be found here: \url{https://github.com/CompEvol/beast2/blob/9f040ed0357c4b946ea276a481a4c654ad4fff36/src/beast/core/util/ESS.java#L161}
#' @author The original Java version of the algorithm was from Remco Bouckaert,
#'   ported to R and adapted by Richel J.C. Bilderbeek
calc_act <- function(trace, sample_interval) {
  beastier::calc_act(trace, sample_interval)
}
