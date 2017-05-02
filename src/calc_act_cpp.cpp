#include "calc_act_cpp.h"

#include <cmath>
#include <stdexcept>

double calc_act_cpp(const std::vector<double>& trace, const int sample_interval)
{
  //return static_cast<double>(sample.size()) * static_cast<double>(sample_interval);
  if (sample_interval < 1) {
    throw std::invalid_argument("sample interval must be at least one");
  }

  // A constant I found in the original class
  const int max_lag = 2000;

  // sum of trace, excluding burn-in
  double sum = 0.0;
  //  keep track of sums of trace(i)*trace(i_+ lag) for all lags, excluding burn-in # nolint
  std::vector<double> square_lagged_sums(max_lag, 0.0);
  std::vector<double> auto_correlation(max_lag, 0.0);

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
    for (int lagIndex = 0; lagIndex != std::min(i + 1, max_lag); ++lagIndex)
    {
      square_lagged_sums[lagIndex] = square_lagged_sums[lagIndex] + trace[i - lagIndex] * trace[i];
      // The following line is the same approximation as in Tracer
      // (valid since mean *(samples - lag), sum1, and sum2 are approximately the same)
      // though a more accurate estimate would be
      // autoCorrelation[lag] = m_fSquareLaggedSums.get(lag) - sum1 * sum2
      auto_correlation[lagIndex] = square_lagged_sums[lagIndex] - (sum1 + sum2) * mean + mean * mean * (i + 1 - lagIndex);
      auto_correlation[lagIndex] /= (i + 1 - lagIndex);
      sum1 -= trace[i - lagIndex];
      sum2 -= trace[lagIndex];
    }
  }


        const int maxLag = std::min(static_cast<int>(trace.size()), max_lag);
        double integralOfACFunctionTimes2 = 0.0;
        for (int lagIndex = 0; lagIndex < maxLag; lagIndex++)
        {
            if (lagIndex == 0)
            {
                integralOfACFunctionTimes2 = auto_correlation[0];
            }
            else if (lagIndex % 2 == 0)
            {
                // fancy stopping criterion - see main comment in Tracer code of BEAST 1
                if (auto_correlation[lagIndex - 1] + auto_correlation[lagIndex] > 0)
                {
                    integralOfACFunctionTimes2 += 2.0 * (auto_correlation[lagIndex - 1] + auto_correlation[lagIndex]);
                }
                else
                {
                    // stop
                    break;
                }
            }
        }

  // auto correlation time
  return sample_interval * integralOfACFunctionTimes2 / auto_correlation[0];
}
