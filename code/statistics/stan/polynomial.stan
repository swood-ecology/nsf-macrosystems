data {
  int<lower=0> N; // number of cases
  vector[N] x;    // moisture treatment (covariate)
  vector[N] y;    // CO2 flux (variate)
}

transformed data {
  vector[N] x2 = square(x); // squared term of moisture treatment
}

parameters {
  real alpha; // intercept
  real beta1; // slope for linear trend
  real beta2; // curvature
  real<lower=0> sigma; // outcome noise
}

model {
  // Prios 
  alpha ~ normal(0, 10);
  beta1 ~ normal(0, 10);
  beta2 ~ normal(0, 10);
  sigma ~ cauchy(0, 5);
  
  // Model
  y ~ normal(alpha + beta1 * x + beta2 * x2, sigma);
}

