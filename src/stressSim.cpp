#include <Rcpp.h>
#include "stress.h"
using namespace Rcpp;

//' Simulates stress using a offset and value vector
//'
//' @param offsets Offsets of the values used to indicate spikes
//' @param values Spike values used to increase stress
//' @param decay_ms Stress decay per ms
//' @param decay_perc_s Stress decay per second
//' @param stress_init Initial Stress
//'
//' @export
// [[Rcpp::export(name=".cppSimulateKey")]]
DataFrame cppSimulateKey(NumericVector offsets,
                         NumericVector values,
                         double decay_ms = 0.0,
                         double decay_perc_s = 0.1,
                         double stress_init = 0.0) {

  Stress stress(stress_init, decay_ms, decay_perc_s);

  unsigned int rows = offsets.length();

  // Assert length
  assert((values.length() == rows,
          "Both vectors must be equal in length."));

  // Initialize with -1 as default
  NumericVector stress_out(rows, -1.0);

  double offset_buffer = 0.0;
  double offset = 0.0;
  double duration = 0.0;

  for (unsigned int row = 0; row < rows; row ++) {
    offset = offsets[row];
    duration = offset - offset_buffer;

    stress.decay(duration);
    stress.spike(values[row]);
    stress_out[row] = stress.value();

    offset_buffer = offset;
  }

  DataFrame stress_df = DataFrame::create(
    _["offsets"] = clone(offsets),
    _["stress"] = clone(stress_out)
  );

  return stress_df;
}
