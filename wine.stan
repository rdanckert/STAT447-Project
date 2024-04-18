

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;  // number of wines
  int<lower=0> N_valid; // number of wines to predict with
  vector<lower=-4,upper=2>[N] body; // body rating
  vector<lower=-8, upper=1>[N] acidity; // acidity rating
  vector<lower=-1>[N] price; // price of the wine
  vector<lower=-1, upper=1>[N] y; // quality rating
  vector<lower=-4,upper=2>[N_valid] body_pred; // predictors
  vector<lower=-8, upper=1>[N_valid] acidity_pred;
  vector<lower=-1>[N_valid] price_pred;
}

parameters {
  real b1; // body slope
  real b2; // acidity slope
  real b3; // price slope
  real intercept; // model intercept
  real <lower=0>sigma; // standard deviation of linear model
}

model {
  // prior
  b1 ~ normal(0,0.1);
  b2 ~ normal(0,0.1);
  b3 ~ normal(0,0.1);
  intercept ~ normal(0, 0.3);
  sigma ~ exponential(0.1);
  
  // likelihood
  y ~ normal(intercept + b1*body + b2*acidity + b3*price, sigma^2); 
}

generated quantities {
  vector[N_valid] quality_pred;
  for (i in 1:N_valid)
    quality_pred[i] = normal_rng(body_pred[i]*b1 + acidity_pred[i]*b2 + price_pred[i]*b3 + intercept, sigma);
    
}