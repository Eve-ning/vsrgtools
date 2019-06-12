#pragma once
#include <Rcpp.h>
using namespace Rcpp;

// This is where CPP specific parameters are specified
// The reason for this being implemented in CPP is due
// to how it improves performance by multiple folds.

class Params {
  
private:
  // note, lnoteh and lnotel names must be present if 
  // the mapping is used!
  List mapping = List::create(
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

public:
  
  double decay_choice(std::string choice = "basic");
  // Required: double stress, double duration only
  double decay_func(double stress,
                    double duration,
                    double alpha,
                    double beta);

  // Required: double stress, with additional arguments
  double spike_func(double stress,
                    std::string type);

};