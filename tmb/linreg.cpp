#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
  {
  DATA_VECTOR(y);
  DATA_VECTOR(x);
  int n = y.size();
  
  PARAMETER(b0);
  PARAMETER(b1);
  PARAMETER(logsigma);
  // Create objectes
  vector<Type> yfit(n);
  Type sigma;
  sigma = exp(logsigma);
  Type neglogL = 0.0;
  
  for(int i = 0; i < n; i++) { //indexing starts at zero, i through n
    yfit(i) = b0 + b1 * x(i);
    neglogL += -dnorm(y(i), yfit(i), sigma, true);
    
  }
  
  
  return neglogL;
  }

