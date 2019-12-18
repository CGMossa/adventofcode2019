#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
std::vector<double> process(NumericVector calculation) {
  // NumericVector result (calculation.length(), 0.0);
  std::vector<double> result;
  // result.capacity(calculation.length());

  std::accumulate(calculation.cbegin(),
                  calculation.cend(),
                  0.0,
                  [&result](double x, double y){
                    auto new_entry = std::max(0.0, x + y);
                    result.push_back(new_entry);
                    return new_entry;
                  });

  return result;
}
// [[Rcpp::export]]
std::vector<double> process_with_cap(NumericVector calculation) {
  std::vector<double> result;
  result.reserve(calculation.length());

  std::accumulate(calculation.cbegin(),
                  calculation.cend(),
                  0.0,
                  [&result](double x, double y){
                    auto new_entry = std::max(0.0, x + y);
                    result.push_back(new_entry);
                    return new_entry;
                  });

  return result;
}


/*** R
# calculation %>% process()
# answer
*/
