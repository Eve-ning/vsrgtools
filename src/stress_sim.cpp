#include <Rcpp.h>
#include "parameters.h"
using namespace Rcpp;

// Simulates the chart on CPP rather than R
// Input
  // Note that the first 2 arguments must match in length
  // offsets: Offsets in a vector
  // is_spikes: Logical Vector indicating if it's a spike
  // args_list: A List indicating different arguments for
  //  spike vs. decay
  // spike & decay_func: Spike and Decay Functions, the 
  //  arguments must match those provided in args_list
  // stress: The value to initialize stress with
// Output
  // A list containing stress_base and stress_spike, 
  //  accessible via "base" & "spike" names
// [[Rcpp::export]]

DataFrame cpp_simulate_key(NumericVector offsets,
                           CharacterVector types,
                           double decay_alpha = 1.5,
                           double decay_beta = 1000,
                           double stress = 0.0) {
  
  Params params;
  
  unsigned int rows = offsets.length();
  
  // Assert length
  assert((types.length() == rows,
          "Both vectors must be equal in length."));
  
  // Initialize with -1 as default
  NumericVector stress_base(rows, -1.0);
  // NumericVector stress_decay(rows, -1.0);
  
  double offset_buffer = 0.0;
  double offset = 0.0;
  double duration = 0.0;
  std::string type = "";
  
  for (unsigned int row = 0; row < rows; row ++) {
    // Get all the required parameters
    offset = offsets[row];
    type = types[row];
    duration = offset - offset_buffer; 
    
    // If spike, we will calculate via decay then spike_func
    // and commit into stress var.
    // Else, will calculate via decay_func but do not commit.
    
    // This conditional is reversed since non-spikes are more
    // common
    if (type == "NA") {
      stress_base[row] = params.decay_func(stress,
                                           duration,
                                           decay_alpha,
                                           decay_beta);
    } else {
      stress = params.decay_func(stress,
                                 duration,
                                 decay_alpha,
                                 decay_beta);
      
      // stress_decay[row] = stress;
      stress = params.spike_func(stress, type);
      stress_base[row] = stress;
    }

    offset_buffer = offset;
  }
  
  DataFrame stress_df = DataFrame::create(
    _["offsets"] = clone(offsets),
    _["stress"] = clone(stress_base)
    // _["stress_decay"] = clone(stress_decay)
  );
  
  return stress_df;
}