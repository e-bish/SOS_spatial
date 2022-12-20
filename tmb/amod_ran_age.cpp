// Simple armor model with random effects
#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  //specify data
  DATA_VECTOR(y); //fish counts
  DATA_MATRIX(X); //fixed effects data matrix
  DATA_MATRIX(Z); //random effects (site) data matrix
  DATA_MATRIX(L); //shoreline type data matrix
  DATA_VECTOR(a); //restoration age
  int n = y.size();
  
  //specify parameters
  PARAMETER_VECTOR(beta); //fixed effect coefficients
  PARAMETER_VECTOR(gamma); //random effect coefficients
  PARAMETER(lambda_nat); //effects of natural shoreline
  PARAMETER(lambda_arm); //effects of armored shoreline
  PARAMETER(log_b); //effect of restoration age
  PARAMETER(log_var); //variance
  
  //create objects
  vector<Type> lambda(3);
  lambda[0] = lambda_nat;
  lambda[1] = lambda_arm;
  vector<Type> mu(n); 
  vector<Type> logmu(n); 
  Type b = exp(log_b);
  Type var = exp(log_var);
  Type neglogL = 0.0; //initial starting value for neg log likelihood
  
  //negative log likelihood
  for(int i = 0; i < n; i++) { 
    
    //lambda_rest is a function of lambda_arm and restoration age
     lambda(2) = lambda[1]*exp(-b*a[i]); 
    
    //objective function
    vector<Type> xtmp = X.row(i);
    vector<Type> ztmp = Z.row(i);
    vector<Type> ltmp = L.row(i);
      
    logmu(i) = (xtmp * beta).sum(); + (ztmp * gamma).sum() + (ltmp * lambda).sum(); //on the whiteboard we had lambda[i] but doesn't that not work because it has a different number of elements? 
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



















































































































































































































































