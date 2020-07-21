data {
  int<lower=0> N;       // number of cases in y
  int<lower=0> R;       // number of replicates per sample
  vector[N] y;          // outcome (variate)
  vector[R] a_meas[N];  // measurement of a (co-variate)
  vector[R] b_meas[N];  // measurement of b (co-variate)
}

parameters {
  vector[N] z_a;  
  vector[N] mu_a;             // prior location
  vector<lower=0>[N] sigma_a; // prior scale

  vector[N] z_b;  
  vector[N] mu_b;             // prior location
  vector<lower=0>[N] sigma_b; // prior scale
  
  real mm_a;
  real<lower=0> ms_a;

  real mm_b;
  real<lower=0> ms_b;

  # Model parameters
  real alpha; // intercept
  real beta; // slope
  real<lower=0> sigma; // outcome noise
}

transformed parameters {
  // non-centered parameterization for a and b
  vector[N] a = mu_a + sigma_a .* z_a; // unknown true value for a  
  vector[N] b = mu_b + sigma_b .* z_b; // unknown true value for b  
  vector[N] x = a ./ b; 
}

model {
  z_a ~ std_normal();
  z_b ~ std_normal();

  y ~ normal(alpha + beta * x, sigma);
  
  for (n in 1:N){
    a_meas[n] ~ normal(mu_a[n], sigma_a[n]);
    b_meas[n] ~ normal(mu_b[n], sigma_b[n]);
  }
  
  # Data priors
  mu_a ~ normal(0, 10);
  mu_b ~ normal(0, 10);

  sigma_a ~ exponential(1);
  sigma_b ~ exponential(1);
}

