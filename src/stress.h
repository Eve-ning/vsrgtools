#pragma once
#include <Rcpp.h>
using namespace Rcpp;

class Stress {
public:

  Stress(double value,
         double decay_ms);

  void decay(double duration);

  void spike(double value);

  double value() const;
  void setValue(double value);

private:

  double m_value;
  double m_decay_ms;
};
