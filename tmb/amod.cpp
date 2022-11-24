// Simple armor model with no random effects
#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  //specify data
  DATA_VECTOR(y);
  DATA_VECTOR(x);
  DATA_VECTOR(z);
  int n = y.size();
  
  //specify parameters
  PARAMETER(b1);
  PARAMETER(b2);
  PARAMETER(log_var);

  //create objects
  vector<Type> mu(n); 
  Type var=exp(log_var);
  Type neglogL = 0.0; //initial starting value for neg log likelihood

  //negative log likelihood
  for(int i = 0; i < n; i++) { 
    mu(i) = exp(b1*x(i) + b2*z(i));
    neglogL -= dnbinom2(y(i), mu(i), mu(i)*(1.0+var), true);
  }
  
  //stuff to report 
  return neglogL;
  ADREPORT(var);
  ADREPORT(mu);
}



















































































































































































































































