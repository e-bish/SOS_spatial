// Simple armor model with random effects
#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  //specify data
  DATA_VECTOR(y);
  DATA_MATRIX(X);
  DATA_MATRIX(Z);
  DATA_MATRIX(L);
  int n = y.size();
  
  //specify parameters
  PARAMETER_VECTOR(beta); // fixed effect coefficients
  PARAMETER_VECTOR(gamma); // random effect coefficients
  PARAMETER_VECTOR(lambda); // effects of shoreline type
  //PARAMETER_VECTOR(log_b);
  PARAMETER(log_var);
  
  //create objects
  vector<Type> mu(n); 
  vector<Type> logmu(n); 
  //Type b = exp(log_b);
  Type var = exp(log_var);
  Type neglogL = 0.0; //initial starting value for neg log likelihood
  
  //objective function
  logmu = X * beta + Z * gamma + L * lambda;
  
  //negative log likelihood
  for(int i = 0; i < n; i++) { 
    mu(i) = exp(logmu(i));
    neglogL -= dnbinom2(y(i), mu(i), mu(i)*(1.0+var), true);
  }
  
  //stuff to report 
  return neglogL;
  ADREPORT(var);
  ADREPORT(mu);
}



















































































































































































































































