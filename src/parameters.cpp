#include <Rcpp.h>
#include "parameters.h"
using namespace Rcpp;

// This is where CPP specific parameters are specified
// The reason for this being implemented in CPP is due
// to how it improves performance by multiple folds.

// Required: double stress, double duration only
// [[Rcpp::plugins(cpp11)]]
double Params::decay_func(double stress,
                          double duration,
                          double decay_ms) {
  // This is defined directly
  double out = stress - (decay_ms * duration);
  return out < 0 ? 0 : out;
}

// Required: double stress, with additional arguments
double Params::spike_func(double stress,
                          std::string type) {

  // Note the cast before the 2nd indexing, it is required.
  double adds = as<NumericVector>(mapping["adds"])[type];
  double mults = as<NumericVector>(mapping["mults"])[type];

  return (stress + adds) * mults;

  // Template
  // <type> <name> = as<<vectorClass>>(df_mapping["<name>"])[type];
}


