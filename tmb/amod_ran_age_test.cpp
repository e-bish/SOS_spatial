// Simple armor model with random effects
#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  //specify data
  DATA_VECTOR(y); //fish counts
  DATA_MATRIX(X); //fixed effects data matrix
  DATA_MATRIX(Z); //random effects (site) data matrix
  DATA_MATRIX(L1); //shoreline type data matrix
  DATA_MATRIX(L2); //shoreline type data matrix
  DATA_VECTOR(a); //restoration age
  int n = y.size();
  
  //specify parameters
  PARAMETER_VECTOR(beta); //fixed effect coefficients
  PARAMETER_VECTOR(gamma); //random effect coefficients
  PARAMETER_VECTOR(lambda); //effects of armored and natural shoreline
  PARAMETER(lambda_rest); //effects of restored shoreline
  PARAMETER(log_b); //effect of restoration age
  PARAMETER(log_var); //variance
  
  //create objects
  vector<Type> mu(n); 
  vector<Type> logmu(n); 
  Type b = exp(log_b);
  Type var = exp(log_var);
  Type neglogL = 0.0; //initial starting value for neg log likelihood
  
  //negative log likelihood
  for(int i = 0; i < n; i++) { 
    
    //lambda_rest is a function of lambda_arm and restoration age
    lambda_rest = lambda[0]*exp(-b*a[i]); 
    
    //objective function
    logmu(i) = X(i) * beta + Z(i) * gamma + L1(i) * lambda + L2(i) * lambda_rest(i); 
    
    //mean mu for each row
    mu[i] = exp(logmu[i]);
    
    //minimize
    neglogL -= dnbinom2(y[i], mu[i], mu[i]*(1.0+var), true);
  }
  
  //stuff to report 
  return neglogL;
  ADREPORT(var);
  ADREPORT(mu);
  ADREPORT(b);
}



















































































































































































































































