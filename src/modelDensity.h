#include <Rcpp.h>
using namespace Rcpp;

// Header declaration for this function so that other functions
// can use this
NumericVector cppModelDensity(NumericVector unq_offsets,
                              NumericVector obj_offsets,
                              double window,
                              bool is_sorted = false);
