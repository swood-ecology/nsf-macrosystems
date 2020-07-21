data {
  int<lower=0> N;     // number of cases
  vector[N] y_obs;   // measurement of y
  real<lower=0> sd_known;  // measurement noise
  vector[N] x;        // predictor
}

transformed data {
  vector[N] x2 = square(x); // squared term of moisture treatment
}

parameters{
  real alpha;
  real beta1;
  real beta2;
  real<lower=0> sd_parameter;
}

transformed parameters {
  real sd_total = sqrt(sd_known ^ 2 + sd_parameter ^ 2);
}

model {
  # Priors 
  alpha ~ normal(0, 10);
  beta1 ~ normal(0, 10);
  beta2 ~ normal(0, 10);
  sd_parameter ~ normal(5, 10);

  # Likelihood
  y_obs ~ normal(alpha + x * beta1 + x2 * beta2, sd_total);
} 

generated quantities {
    vector[N] y_true = alpha + x * beta1 + x2 * beta2;
}
