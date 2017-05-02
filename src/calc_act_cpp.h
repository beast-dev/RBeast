#ifndef CALC_ACT_CPP_H
#define CALC_ACT_CPP_H

#include <vector>

//' Does magic
//' @param sample sample
//' @return sample_interval sample interval
//' @return the ACT
// [[Rcpp::export]]
double calc_act_cpp(const std::vector<double>& sample, const int sample_interval);

#endif // CALC_ACT_CPP_H


