/* Beta-Binomial Hierarchical Model in STAN */
data {
  int<lower=0> N;        // Number of bags
  array[N] int<lower=0> n;     // Number of marbles drawn from each bag 
  array[N] int<lower=0> y;     // Number of black marbles in each bag
}

parameters {
  real<lower=0, upper=1> mu;  // Mean of Beta
  real<lower=0> phi;           // Precision of Beta
}

transformed parameters {
  // Precompute alpha and beta for Beta-Binomial
  real alpha = mu * phi;
  real beta  = (1 - mu) * phi;
}

model {
  // Priors
  mu  ~ beta(1, 1);        // weakly informative uniform alternative
  phi ~ exponential(1);

  // Likelihood (Beta-Binomial)
  y ~ beta_binomial(n, alpha, beta);
}

generated quantities {
  // posterior samples for theta
  vector[N] theta;
  for (i in 1:N) {
    theta[i] = beta_rng(alpha + y[i], beta + n[i] - y[i]);
  }
}
