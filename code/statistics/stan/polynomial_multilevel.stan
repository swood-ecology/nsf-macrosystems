data {
  int<lower=0> N; // number of cases
  int<lower=0> J; // number of hierarchies 
  vector[N] moistTreat;    // moisture treatment (covariate)
  vector[N] moistPlot; // initial gravimetric (quadrat moisture) (ALSO ADD MEASUREMENT ERROR IN X)
  vector[N] y;    // CO2 flux (variate)
  
  // Random effect
  int site[N];
}

transformed data {
  vector[N] x2 = square(moistTreat); // squared term of moisture treatment
}

parameters {
  vector[J] alpha; // intercept
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
  for(n in 1:N)
    y[n] ~ normal(alpha[site[n]] + betaMoistTreat * moistTreat[n] + betaMoistTreatSq * x2[n] + betaMoistPlot * moistPlot[n], sigma);
}

