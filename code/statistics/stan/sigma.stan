data {
  int<lower=0> N;     // number of cases
  vector[N] x_meas;   // measurement of x
  real<lower=0> tau;  // measurement noise
  vector[N] y;        // outcome (variate)
}

parameters {
  vector[N] x;    // unknown true value
  real mu_x;    // prior location
  real sigma_x; // prior scale
  
  real alpha;   // intercept
  real beta;    // slope
  real<lower=0> sigma; // outcome noise
}

model {
  # Priors 
  alpha ~ normal(0, 10);
  beta ~ normal(0, 10);
  sigma ~ cauchy(0, 5);

  # Model
  x ~ normal(mu_x, sigma_x);
  x_meas ~ normal(x, tau);
  y ~ normal(alpha + beta * x, sigma); 
}
