// Simple armor model with no random effects
#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  //specify data
  DATA_VECTOR(y);
  DATA_MATRIX(X);
  DATA_MATRIX(Z);
  int n = y.size();
  
  //specify parameters
  PARAMETER_VECTOR(beta); // fixed effect coefficients
  PARAMETER_VECTOR(gamma); // random effect coefficients
  PARAMETER(log_var);
  
  //create objects
  vector<Type> mu(n); 
  vector<Type> logmu(n); 
  Type var = exp(log_var);
  Type neglogL = 0.0; //initial starting value for neg log likelihood
  
  logmu = X * beta + Z * gamma;
  
  //negative log likelihood
  for(int i = 0; i < n; i++) { 
    mu(i) = exp(logmu(i));
    neglogL -= dnbinom2(y(i), mu(i), var, true);
  }
  
  //stuff to report 
  return neglogL;
  ADREPORT(var);
  ADREPORT(mu);
}



















































































































































































































































