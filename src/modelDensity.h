#include <Rcpp.h>
using namespace Rcpp;

// Header declaration for this function so that other functions
// can use this

NumericVector cppCountForward(NumericVector unq_offsets,
                              NumericVector obj_offsets,
                              double window,
                              bool is_sorted = false);

List cppModelDensity(NumericVector unq_offsets,
                     NumericVector obj_offsets,
                     double window,
                     bool is_sorted = false);


