#pragma once
#include <Rcpp.h>
using namespace Rcpp;

// This is where CPP specific parameters are specified

// [[Rcpp::plugins(cpp11)]]
class Params {

public:

  Params();

  double decay_choice(std::string choice = "basic");
  // Required: double stress, double duration only
  double decay_func(double stress,
                    double duration,
                    double decay_ms);

  // Required: double stress, with additional arguments
  double spike_func(double stress,
                    std::string type);

private:

  List mapping;

};
