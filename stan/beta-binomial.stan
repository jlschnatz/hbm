// Beta-Binomial Hierarchical Model in STAN
data {
  int<lower=0> N;           // Number of bags
  array[N] int<lower=0> n;  // Number of marbles drawn from each bag
  array[N] int<lower=0> y;  // Number of black marbles in each bag
}

parameters {
  real<lower=0,upper=1> mu;               // Hyperparameter: mean of the Beta distribution              
  real<lower=0> phi;                      // Hyperparameter: precision of the Beta distribution               
  array[N] real<lower=0, upper=1> theta;  // Bag-specific proportion of black marbles
}

transformed parameters {
  // Reparameterization of the Beta distribution
  real<lower=0> alpha = mu * phi;
  real<lower=0> beta  = (1 - mu) * phi;
}

model {
  mu  ~ uniform(0, 1);        // Hyperprior for µ     
  phi ~ exponential(1);       // Hyperprior for ϕ   
  theta ~ beta(alpha, beta);  // Conditional prior for θ
  y ~ binomial(n, theta);     // Likelihood
}
