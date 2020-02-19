data {
  int<lower=0> N; // number of cases
  vector[N] x; // predictor (covariate)
  vector[N] y; // outcome (variate)
}

parameters {
  real alpha; // intercept
  real beta; // slope
  real<lower=0> sigma; // outcome noise
}

model {
  # Prios 
  alpha ~ normal(0, 10);
  beta ~ normal(0, 10);
  sigma ~ cauchy(0, 5);
  
  # Model
  y ~ normal(alpha + beta * x, sigma);
}

