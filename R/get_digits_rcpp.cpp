#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector timesTwo(NumericVector x) {
  return x * 2;
}

/*** R

get_digit <- function(x, d) {
  # digits from the right
  # i.e.: first digit is the ones, second is the tens, etc.
  (x %% 10 ^ d) %/% (10 ^ (d - 1))
}


*/
