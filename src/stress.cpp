#include <Rcpp.h>
#include "stress.h"
using namespace Rcpp;

// Constructor
Stress::Stress(double decay_ms) {
  m_decay_ms = decay_ms;
}

// Trigger Decay
double Stress::decay(double stress,
                     double duration) {
  double out = stress - (m_decay_ms * duration);
  return out < 0 ? 0 : out; // ensure stress doesn't go below 0
}

// Trigger Spike
double Stress::spike(double stress,
                     double value) {
  return stress + value;
}
