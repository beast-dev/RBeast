#' Calculate the auto-correlation time
#' @param trace the values
#' @param sample_interval the interval in timesteps between samples
#' @return the auto_correlation time
#' @export
#' @seealso Java code can be found here: \url{https://github.com/CompEvol/beast2/blob/9f040ed0357c4b946ea276a481a4c654ad4fff36/src/beast/core/util/ESS.java#L161}
#' @author The original Java version of the algorithm was from Remco Bouckaert,
#'   ported to R and adapted by Richel Bilderbeek
calc_act <- function(trace, sample_interval) {
  if (!is.numeric(trace)) {
    stop("trace must be numeric")
  }
  if (sample_interval < 1) {
    stop("sample interval must be at least one")
  }

  # The code is adapted from Java, that has arrays starting at index zero,
  # where array indices in R start at index one.
  # I kept as close to the Java code as possible, just adding
  # '+ 1' within index operators.
  # I kept in the original comments as well

  # A constant I found in the original class
  max_lag <- 2000

  # sum of trace, excluding burn-in
  sum <- 0.0
  #  keep track of sums of trace(i)*trace(i_+ lag) for all lags, excluding burn-in # nolint
  square_lagged_sums <- rep(0.0, times = max_lag)
  auto_correlation <- rep(0.0, times = max_lag)

  for (i in seq(0, length(trace) - 1)) {
    sum <- sum + trace[i + 1]
    # calculate mean
    mean <- sum / (i + 1)

    # calculate auto correlation for selected lag times
    # sum1 = \sum_{start ... totalSamples-lag-1} trace # nolint
    sum1 <- sum
    # sum2 = \sum_{start+lag ... totalSamples-1} trace # nolint
    sum2 <- sum

    for (lag_index in seq(0, min(i + 1, max_lag) - 1)) {
      testit::assert(lag_index + 1 >= 1)
      testit::assert(lag_index + 1 <= length(square_lagged_sums))
      testit::assert(i + 1 >= 1)
      testit::assert(i + 1 <= length(trace))
      testit::assert(i - lag_index + 1 >= 1)
      testit::assert(i - lag_index + 1 <= length(trace))
      testit::assert(lag_index + 1 <= length(trace))
      square_lagged_sums[lag_index + 1] <- square_lagged_sums[lag_index + 1] +
        trace[i - lag_index + 1] * trace[i + 1]
      # The following line is the same approximation as in Tracer
      # (valid since mean *(samples - lag), sum1, and sum2 are approximately the same) # nolint
      # though a more accurate estimate would be
      # auto_correlation[lag] = m_fsquare_lagged_sums.get(lag) - sum1 * sum2  # nolint
      testit::assert(lag_index + 1 >= 1)
      testit::assert(lag_index + 1 <= length(auto_correlation))

      auto_correlation[lag_index + 1] <- square_lagged_sums[lag_index + 1] - (sum1 + sum2) * mean + mean * mean * (i + 1 - lag_index) # nolint
      testit::assert(i + 1 - lag_index != 0)
      auto_correlation[lag_index + 1] <- auto_correlation[lag_index + 1] / (i + 1 - lag_index) # nolint
      sum1 <- sum1 - trace[i - lag_index + 1]
      sum2 <- sum2 - trace[lag_index + 1]
    }
  }

  max_lag <- min(length(trace), max_lag)
  integral_ac_times_two <- 0.0
  for (lag_index in seq(0, max_lag - 1)) {
    if (lag_index == 0) {
      integral_ac_times_two <- auto_correlation[0 + 1]
    } else if (lag_index %% 2 == 0) {
      # fancy stopping criterion - see main comment in Tracer code of BEAST 1
      if (auto_correlation[lag_index - 1 + 1] +
          auto_correlation[lag_index + 1] > 0
      ) {
        integral_ac_times_two <- integral_ac_times_two +
          (2.0 * (auto_correlation[lag_index - 1 + 1] +
          auto_correlation[lag_index + 1]))
      } else {
        break
      }
    }
  }

  # auto correlation time
  act <- sample_interval * integral_ac_times_two / auto_correlation[1]
  act
}
