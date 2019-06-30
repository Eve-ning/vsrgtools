#include <Rcpp.h>
using namespace Rcpp;

//' Broadcasts the difference between resets provided
//'
//' @description Offsets helps indicate the distances
//' between the resets.
//'
//' For Example, R: Reset, -: Nothing
//'
//' [1][2][3][4]
//'  R  -  -  - [9]
//'  -  R  -  - [7]
//'  R  -  R  R [3]
//'  -  R  R  R [1]
//'
//' This will give
//'
//' [1][2][3][4]
//'  0  0  0  0 [9]
//'  2  2  2  2 [7]
//'  6  4  6  6 [3]
//'  2  6  2  2 [1]
//'
//' Notice how in output, it is able to show the difference
//' between each successive pair of resets.
//'
//' @param offsets a NumericVector indicating the offsets
//' of all of the resets.
//'
//' This must be provided in a descending order, also
//' with respect to resets
//' @param resets a LogicalMatrix indicating the resets
//'
//' This must be provided in a descending order with respect
//' to offsets
//'
//' @export
// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::export(name=".cppBroadcast")]]
NumericMatrix cppBroadcast(NumericVector offsets,
                           LogicalMatrix resets){

  unsigned int rows = offsets.length();

  assert((rows != resets.nrows(),
          "Number of rows of offsets and resets must match."));

  unsigned int keys = resets.ncol();

  NumericVector trackers(keys, 0);
  LogicalVector resets_row(keys, false);

  // This is a preallocated output matrix
  NumericMatrix output(resets.nrow(), resets.ncol());

  double offset_buffer = 0.0;
  double offset = 0.0;
  double offset_diff = 0.0;

  for (unsigned int row = 0; row < rows; row ++) {
    offset = offsets[row];
    resets_row = resets.row(row); // Matrix needs to access via row()
    offset_diff = offset_buffer - offset;

    trackers = trackers + offset_diff;

    // Assign to output
    output(row,_) = trackers;

    // Reset trackers based on Logi
    for (unsigned int reset = 0; reset < keys; reset ++) {
      if (resets_row[reset]) {
        trackers[reset] = 0;
      }
    }

    offset_buffer = offset;
  }

  return output;
};
