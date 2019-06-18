#include <Rcpp.h>
#include "parameters.h"
using namespace Rcpp;

// This is where CPP specific parameters are specified
// The reason for this being implemented in CPP is due
// to how it improves performance by multiple folds.

// Required: double stress, double duration only
// [[Rcpp::plugins(cpp11)]]

Params::Params(){
  // note, lnoteh and lnotel names must be present if
  // the mapping is used!
  mapping = List::create(
    _["adds"]  = NumericVector::create(
      _["note"] = 0.5,
      _["lnoteh"] = 0.5,
      _["lnotel"] = 0.2),
      _["mults"] = NumericVector::create(
        _["note"] = 1.0,
        _["lnoteh"] = 1.0,
        _["lnotel"] = 1.0)
  // _["<name>"] = <vectorClass>(
  //   _["note"] = <param>,
  //   _["lnoteh"] = <param>,
  //   _["lnotel"] = <param>)
  );
}
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
