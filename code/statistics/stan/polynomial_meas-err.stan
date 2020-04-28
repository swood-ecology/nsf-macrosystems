data {
  int<lower=0> N;     // number of cases
  vector[N] y_obs;      // measurement of y
  vector[N] x;          // predictor
  vector[N] y_err;      // error in y
}

transformed data {
  vector[N] x2 = square(x); // squared term of moisture treatment
}

parameters {
  vector[N] y;
  real alpha; // intercept
  real beta1; // slope for linear trend
  real beta2; // curvature
  real<lower=0> sigma; // outcome noise
}

model {
  vector[N] mu;

  // Priors 
  sigma ~ cauchy( 0 , 2.5 );
  beta1 ~ normal( 0 , 10 );
  beta2 ~ normal( 0 , 10 );
  alpha ~ normal( 0 , 10 );
  y_obs ~ normal( y , y_err );
  
  // Model
  for (i in 1:N) {
    mu[i] = alpha + beta1 * x[i] + beta2 *x2[i];  
  }
  y ~ normal( mu , sigma );
  
}

generated quantities{
  vector[N] mu;
  for ( i in 1:N ) {
      mu[i] = alpha + beta1 * x[i] + beta2 * x2[i];
  }
}
