data {
  int<lower=0> N;     // number of cases
  vector[N] y_meas;   // measurement of y
  real<lower=0> sd_known;  // measurement noise
  vector[N] x;        // predictor
}

parameters{
  real alpha ;
  real beta ;
  real<lower=0> sigma ;
  
  real mu;
  real<lower=0> sd_parameter
}

model {
  sd_total = sqrt(sd_known ^ 2 + sd_parameter ^ 2);
  
  # Priors 
  alpha ~ normal(0, 10);
  beta ~ normal(0, 10);
  sigma ~ normal(tau, 0.001) ; //where you'd want to tweak the SD argument to reflect your certainty on tau
  sd_parameter ~ normal(0, 5); //whatever prior you find sensible

  # Likelihood
  mu = alpha + x * beta, sigma;
  y_meas ~ normal(mu, sd_total);
} 

generated quantities{
    vector[N] y_true = alpha + x * beta;
}
