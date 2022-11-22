// Simple armor model with no random effects
#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(total);
  DATA_VECTOR(logyday);
  DATA_VECTOR(X100m);
  PARAMETER(b0);
  PARAMETER(b1);
  PARAMETER(b2);
  PARAMETER(logSigma);
  ADREPORT(exp(2*logSigma));
  Type nll = -sum(dnbinom(total, b0 + b1*logyday + b2*X100m, exp(logSigma), true));
  return nll;
}