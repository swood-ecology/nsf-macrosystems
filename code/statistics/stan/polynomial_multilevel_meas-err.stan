data {
  // True data
  int<lower=0> N;         // number of cases
  int<lower=0> J;         // number of hierarchies 
  vector[N] moistTreat;   // moisture treatment (covariate)

  // Measurement error in y
  vector[N] y_obs;            // CO2 flux (variate)
  vector[N] y_err;            // error in CO2 flux
  // real<lower=0> sd_known;          // measurement noise

  // Measurement error in x
  vector[N] moistPlot_meas;    // initial gravimetric (quadrat moisture) (ALSO ADD MEASUREMENT ERROR IN X)
  real<lower=0> tau;          // measurement noise

  // Variable intercept
  int site[N];
}

transformed data {
  vector[N] x2 = square(moistTreat); // squared term of moisture treatment
}

parameters {
  // True measure of y
  vector[N] y; // unknown true value
  
  // True measure of x
  vector[N] moistPlot;    // unknown true value
  real mu_moistPlot;    // prior location
  real sigma_moistPlot; // prior scale
  
  // Model parameters
  vector[J] alpha; // intercept
  real betaMoistTreat;  // slope for linear trend
  real betaMoistTreatSq;// curvature
  real betaMoistPlot;   // slope for plot moisture effect
  // real<lower=0> sd_parameter;  // outcome noise
  real<lower = 0> sigma;
}

transformed parameters {
  // real sd_total = sqrt(sd_known ^ 2 + sd_parameter ^ 2);
}

model {
  // Priors 
  alpha ~ normal(0, 10);
  betaMoistTreat ~ normal(0, 10);
  betaMoistTreatSq ~ normal(0, 10);
  betaMoistPlot ~ normal(0, 10);
  sigma ~ cauchy( 0 , 10 );
  // sd_parameter ~ normal(5, 10);
  
  // Measurement error in y
  y_obs ~ normal( y , y_err );
  
  // Measurement error in x
  moistPlot ~ normal(mu_moistPlot, sigma_moistPlot);
  moistPlot_meas ~ normal(moistPlot, tau);
  
  // Model
  for(n in 1:N)
    y_obs[n] ~ normal(alpha[site[n]] + betaMoistTreat * moistTreat[n] + betaMoistTreatSq * x2[n] + betaMoistPlot * moistPlot[n], sigma);
}
