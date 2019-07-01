#pragma once
#include <Rcpp.h>
using namespace Rcpp;

class Stress {
public:

  Stress(double value,
         double decay_ms,
         double decay_perc_s);

  void decay(double duration);

  void spike(double value);

  double value() const;
  void setValue(double value);

private:

  double m_value;
  double m_decay_ms;
  double m_decay_perc_s;
};
