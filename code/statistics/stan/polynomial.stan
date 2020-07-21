data {
  int<lower=0> N; // number of cases
  vector[N] moistTreat;    // moisture treatment (covariate)
  vector[N] moistPlot; // initial gravimetric (quadrat moisture) (ALSO ADD MEASUREMENT ERROR IN X)
  vector[N] y;    // CO2 flux (variate)
}

transformed data {
  vector[N] x2 = square(moistTreat); // squared term of moisture treatment
}

parameters {
  real alpha; // intercept
  real betaMoistTreat;  // slope for linear trend
  real betaMoistTreatSq;// curvature
  real betaMoistPlot;   // slope for plot moisture effect
  real<lower=0> sigma;  // outcome noise
}

model {
  // Priors 
  alpha ~ normal(0, 10);
  betaMoistTreat ~ normal(0, 10);
  betaMoistTreatSq ~ normal(0, 10);
  betaMoistPlot ~ normal(0, 10);
  sigma ~ cauchy(0, 5);
  
  // Model
  y ~ normal(alpha + betaMoistTreat * moistTreat + betaMoistTreatSq * x2 + betaMoistPlot * moistPlot, sigma);
}

