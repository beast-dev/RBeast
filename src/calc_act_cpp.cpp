#include "calc_act_cpp.h"

#include <cmath>
#include <stdexcept>

double calc_act_cpp(const std::vector<double>& trace, const int sample_interval)
{
  if (sample_interval < 1) {
    throw std::invalid_argument("sample interval must be at least one");
  }

  // A constant I found in the original class
  const int max_lag = 2000;

  // sum of trace, excluding burn-in
  double sum = 0.0;
  //  keep track of sums of trace(i)*trace(i_+ lag) for all lags, excluding burn-in # nolint
  std::vector<double> square_lagged_sums(max_lag, 0.0);
  // acs: auto correlations
  std::vector<double> acs(max_lag, 0.0);

  for (int i = 0; i != trace.size(); ++i)
  {
    sum += trace[i];
    // calculate mean
    const double mean = sum / static_cast<double>(i + 1);

    // calculate auto correlation for selected lag times
    // sum1 = \sum_{start ... totalSamples-lag-1} trace
    double sum1 = sum;
    // sum2 = \sum_{start+lag ... totalSamples-1} trace
    double sum2 = sum;
    for (int lag_index = 0; lag_index != std::min(i + 1, max_lag); ++lag_index)
    {
      square_lagged_sums[lag_index] = square_lagged_sums[lag_index] + trace[i - lag_index] * trace[i];
      // The following line is the same approximation as in Tracer
      // (valid since mean *(samples - lag), sum1, and sum2 are approximately the same)
      // though a more accurate estimate would be
      // autoCorrelation[lag] = m_fSquareLaggedSums.get(lag) - sum1 * sum2
      acs[lag_index] = square_lagged_sums[lag_index] - (sum1 + sum2) * mean + mean * mean * (i + 1 - lag_index);
      acs[lag_index] /= (i + 1 - lag_index);
      sum1 -= trace[i - lag_index];
      sum2 -= trace[lag_index];
    }
  }


  const int max_lag_ac = std::min(static_cast<int>(trace.size()), max_lag);
  // iactt: integral_of_ac_function_times_2
  double iactt = 0.0;
  for (int lag_index = 0; lag_index != max_lag_ac; ++lag_index)
  {
    if (lag_index == 0)
    {
      iactt = acs[0];
    }
    else if (lag_index % 2 == 0)
    {
      // fancy stopping criterion - see main comment in Tracer code of BEAST 1
      if (acs[lag_index - 1] + acs[lag_index] > 0)
      {
        iactt += 2.0 * (acs[lag_index - 1] + acs[lag_index]);
      }
      else
      {
        // stop
        break;
      }
    }
  }

  // auto correlation time
  return sample_interval * iactt / acs[0];
}
