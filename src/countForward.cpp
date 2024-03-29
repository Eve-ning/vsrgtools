#include <Rcpp.h>
#include "countForward.h"
using namespace Rcpp;

// [[Rcpp::export(name=".cppCountForward")]]
NumericVector cppCountForward(NumericVector unq_offsets,
                              NumericVector obj_offsets,
                              double window,
                              bool is_sorted,
                              Nullable<NumericVector> obj_weights) {

  // Sort them if they are not sorted
  if (!is_sorted){
    unq_offsets = unq_offsets.sort();
    obj_offsets = obj_offsets.sort();
  }

  // Preallocate counts as output
  NumericVector counts_out(unq_offsets.length(), 0.0);

  // We need iterators for both vectors
  double window_start = 0.0;
  double window_end = 0.0;

  double counts = 0.0;

  // We do a top level check so that the check isn't done multiple times
  if (obj_weights.isNotNull()){ // Case when obj_weights is specified
    NumericVector obj_weights_(obj_weights);
    for (int i = 0; i < unq_offsets.length(); i++){
      window_start = unq_offsets[i];
      window_end = window_start + window;
      counts = 0;

      for (int j = 0; j < obj_offsets.length(); j++){
        if (obj_offsets[j] >= window_start &&
            obj_offsets[j] < window_end) {
          counts += obj_weights_[j]; // Here's the difference
        }
      }

      counts_out[i] = counts;
    }
  } else { // Case when obj_weights is not specified
    for (int i = 0; i < unq_offsets.length(); i++){
      window_start = unq_offsets[i];
      window_end = window_start + window;
      counts = 0;

      for (int j = 0; j < obj_offsets.length(); j++){
        if (obj_offsets[j] >= window_start &&
            obj_offsets[j] < window_end) {
          counts += 1; // Here's the difference
        }
      }

      counts_out[i] = counts;
    }
  }
  return counts_out;
}
