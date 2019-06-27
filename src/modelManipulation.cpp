#include <Rcpp.h>
#include "modelDensity.h"
using namespace Rcpp;


//' Helps model.manipulation in speeding up operations
//'
// [[Rcpp::export(name="cppModelManipulation")]]
List cppModelManipulation(NumericVector offsets,
                          List obj_offsets){

  List out = List::create(_["offsets"] = offsets);

  for(int i=0; i<obj_offsets.length(); i++) {
    DataFrame index = as<DataFrame>(obj_offsets[i]);
    out.push_back(cppModelDensity(offsets, index["offsets"], 1000, false));
  }

  return out;
  //
  // DataFrame groups(df.attr("groups"));
  // List rows = groups[groups.size()-1];
  // std::cout << groups.size()-1;
  // int n = groups.nrow();
  // List out;
  //
  //
  //
  // for(int i=0; i<n; i++) {
  //   NumericVector index = rows[i];
  //   out.push_back(cppModelDensity(offsets, index, 1000, false));
  // }
  // return out;
}
