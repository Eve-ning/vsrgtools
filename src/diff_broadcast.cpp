#include <Rcpp.h>
using namespace Rcpp;

// This will be accepting a resetsFrame of
// Input
  // offsets must just be a NumericVector of the offsets
  // resets must a NumericMatrix of the presence of the
  //  broadcasted notes, where broadcasting must reset.
  //  Positive numbers indicate a reset, 0 or lower
  //  otherwise
  
// [[Rcpp::export]]  
NumericMatrix cpp_broadcast(NumericVector offsets,
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