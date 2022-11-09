#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() ()
{
  using namespace density;
  DATA_VECTOR(y); //fish density
  DATA_VECTOR(po2);
  DATA_VECTOR(invtemp);
  DATA_SCALAR(Ao);
  DATA_SCALAR(avgbn);
  DATA_INTEGER(n);
  //DATA_SCALAR(s_cut);

  //Fixed effects parameters
  PARAMETER(Eo); //e0 for metabolic index (mi)
  PARAMETER_ARRAY(b3); //mi effect

  //Tweedie distribution parameters
  PARAMETER(logphi);
  PARAMETER(theta);
  
  //Derived threshold variable
  Type s_slope;
  //Type s_cut;
  s_slope = b3;
  //s_cut = b3(1);
  
  //Derived Tweedie parameters
  Type p;
  p =  1 + exp(theta)/(1 + exp(theta));
  Type phi;
  phi=exp(logphi);
  
  //Number of observations
  //int n = y.size();
  
  
  //Derived variable: mi
  vector<Type> mi(n);
  
  //Predicted MI and catch
  vector<Type> log_yhat(n);
  vector<Type> yhat(n);
  
  
  for(int i = 0; i < n; i++) {
      mi(i) = avgbn * Ao*  po2(i) * exp(Eo * invtemp(i));
    if (mi(i) < s_cut) {
      log_yhat(i) = mi(i) * s_slope;
    } else {
      log_yhat(i) = s_cut * s_slope;
    }
    yhat(i) = exp(log_yhat(i));
  }
  
  Type jnll = -sum(dtweedie(y, yhat, phi, p, true));
  
  //Stuff to return and report
  return jnll;
  
  ADREPORT(p);
  ADREPORT(phi);
  }

