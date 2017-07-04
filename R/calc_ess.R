#' Calculates the Effective Sample Size
#' @param trace the values without burn-in
#' @param sample_interval the interval in timesteps between samples
#' @return the effective sample size
#' @export
#' @author Richel Bilderbeek
calc_ess <- function(trace, sample_interval) {
  if (!is.numeric(trace)) {
    stop("trace must be numeric")
  }
  if (sample_interval < 1) {
    stop("sample interval must be at least one")
  }
  act <- RBeast::calc_act(
    trace = trace,
    sample_interval = sample_interval
  )
  ess <- length(trace) / (act / sample_interval)
  ess
}
