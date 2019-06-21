#include <Rcpp.h>
#include "stress.h"
using namespace Rcpp;

// Constructor
Stress::Stress(double value,
               double decay_ms) {
  m_value = value;
  m_decay_ms = decay_ms;
}

double Stress::value() const {
  return m_value;
}

void Stress::setValue(double value) {
  m_value = value;
}

// Trigger Decay
void Stress::decay(double duration) {
  double new_value = m_value - (m_decay_ms * duration);
  m_value = new_value < 0 ? 0 : new_value; // ensure stress doesn't go below 0
}

// Trigger Spike
void Stress::spike(double value) {
  m_value += value;
}
