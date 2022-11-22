// Simple armor model with no random effects
#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  //specify data
  DATA_VECTOR(y);
  DATA_VECTOR(logyday);
  DATA_VECTOR(X100m);
  int n = y.size();
  
  //specify parameters
  PARAMETER(b0);
  PARAMETER(b1);
  PARAMETER(b2);
  //PARAMETER(log_mu); 
  PARAMETER(log_var);

  //create objects
  vector<Type> yfit(n); 
  //Type mu=exp(log_mu);
  Type var=exp(log_var);
  Type neglogL = 0.0; //initial starting value for neg log likelihood

  //negative log likelihood
  for(int i = 0; i < n; i++) { 
    yfit(i) = b0 + b1 * logyday(i) + b2 * X100m(i);
    neglogL += -dnbinom2(y(i), yfit(i), var, true);
  }
  
  //stuff to report 
  return neglogL;
  ADREPORT(var);
  //ADREPORT(mu);
  //ADREPORT(exp(2*logSigma));  
}



















































































































































































































































