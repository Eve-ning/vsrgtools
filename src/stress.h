#pragma once
#include <Rcpp.h>
using namespace Rcpp;

class Stress {
public:

  Stress(double decay_ms);

  double decay(double stress,
               double duration);

  double spike(double stress,
               double value);

private:

  double m_decay_ms;
};
