#include <Rcpp.h>
#include "countForward.h"
using namespace Rcpp;

//' Helps model.manipulation in speeding up operations
//'
//' @description Calculates how many notes in front of window.
//'
//' This depends heavily on cppCountForward, refer to modelManipulation.R for
//' correct usage.
//'
//' @param unq_offsets a Numeric Vector of the offsets to check on
//' @param obj_offsets a List of the objects offsets
//' @param window a double indicating the window of the check
//' @param is_sorted a boolean indicating if the vectors are sorted, if they
//' aren't, they will be sorted.
//'
//' @export
//'
// [[Rcpp::export(name=".cppModelManipulation")]]
List cppModelManipulation(NumericVector unq_offsets,
                          List obj_offsets,
                          double window = 200,
                          bool is_sorted = false){

  List out = List::create(_["offsets"] = unq_offsets);

  for(int i=0; i<obj_offsets.length(); i++) {
    DataFrame index = as<DataFrame>(obj_offsets[i]);
    out.push_back(cppCountForward(unq_offsets, index["offsets"],
                                  window, is_sorted));
  }

  return out;
}
