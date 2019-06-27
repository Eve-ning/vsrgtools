#include <Rcpp.h>
#include "modelDensity.h"
#include <algorithm>
using namespace Rcpp;

NumericVector cppCountForward(NumericVector unq_offsets,
                              NumericVector obj_offsets,
                              double window,
                              bool is_sorted) {

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

  unsigned int counts = 0;

  for (int i = 0; i < unq_offsets.length(); i++){
    window_start = unq_offsets[i];
    window_end = window_start + window;
    counts = 0;

    for (int j = 0; j < obj_offsets.length(); j++){
      if (obj_offsets[j] >= window_start &&
          obj_offsets[j] < window_end) {
        counts += 1;
      }
    }

    counts_out[i] = counts;
  }

  return counts_out;
}

//' Counts the Density at each point of unq_offsets wrt obj_offsets
//'
//' @description The difference between unq and obj offsets is that
//' unq offsets is the offsets that would be checked on, that is
//' the ones that will show in the output.
//'
//' obj offsets is merely a vector indicating where the objects are
//'
//' @param unq_offsets a Numeric Vector of the offsets to check on
//' @param obj_offsets a Numeric Vector of t he objects offsets
//' @param window a double indicating the window of the check
//' @param is_sorted a boolean indicating if the vectors are sorted
//' , if they aren't, they will be sorted.
//' @export
// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::export(name=".cppModelDensity")]]
List cppModelDensity(NumericVector unq_offsets,
                     List obj_offsets,
                     double window,
                     bool is_sorted) {

  List out = List::create(_["offsets"] = unq_offsets);

  for (int i = 0; i < obj_offsets.length(); i++) {
    DataFrame index = as<DataFrame>(obj_offsets[i]);
    out.push_back(cppCountForward(unq_offsets, index["offsets"], window, is_sorted));
  }

  return out;
}

