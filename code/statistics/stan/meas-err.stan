data {
  int<lower=0> N;     // number of cases
  vector[N] y_obs;   // measurement of y
  real<lower=0> sd_known;  // measurement noise
  vector[N] x;        // predictor
}

parameters{
  real alpha;
  real beta;
  real<lower=0> sd_parameter;
}

transformed parameters {
  sd_total = sqrt(sd_known ^ 2 + sd_parameter ^ 2);
}

model {
  # Priors 
  alpha ~ normal(0, 10);
  beta ~ normal(0, 10);
  sd_parameter ~ normal(5, 1);

  # Likelihood
  y_obs ~ normal(alpha + x * beta, sd_total);
} 

generated quantities {
    vector[N] y_true = alpha + x * beta;
}
