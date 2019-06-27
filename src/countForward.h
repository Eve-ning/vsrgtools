#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector cppCountForward(NumericVector unq_offsets,
                              NumericVector obj_offsets,
                              double window,
                              bool is_sorted = false);
