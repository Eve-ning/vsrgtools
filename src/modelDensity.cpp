#include <Rcpp.h>
#include "countForward.h"
#include <algorithm>
using namespace Rcpp;

//' Counts the Density at each point of unq_offsets wrt obj_offsets
//'
//' @description The difference between unq and obj offsets is that unq offsets
//' is the offsets that would be checked on, that is the ones that will show in
//' the output.
//'
//' obj offsets is merely a vector indicating where the objects are
//'
//' This depends heavily on cppCountForward, refer to modelDensity.R for correct
//' usage
//'
//' @param unq_offsets a Numeric Vector of the offsets to check on
//' @param obj_offsets a List of the objects offsets
//' @param window a double indicating the window of the check
//' @param is_sorted a boolean indicating if the vectors are sorted, if they
//' aren't, they will be sorted.
//'
//' @export
// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::export(name=".cppModelDensity")]]
List cppModelDensity(NumericVector unq_offsets,
                     List obj_offsets,
                     double window,
                     bool is_sorted = false) {

  List out = List::create(_["offsets"] = unq_offsets);

  for (int i = 0; i < obj_offsets.length(); i++) {
    DataFrame index = as<DataFrame>(obj_offsets[i]);
    out.push_back(cppCountForward(unq_offsets, index["offsets"],
                                  window, is_sorted, index["weights"]));
  }

  return out;
}

