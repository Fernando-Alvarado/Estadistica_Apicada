//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

data {
  int<lower=0> n;
  int<lower=0, upper=n> y;
  real mu;
  real<lower=0> sigma;
}

parameters {
  real theta;
}

transformed parameters {
  real<lower=0, upper=1> p;
  p = exp(theta) / (1 + exp(theta));
}

model {
  theta ~ normal(mu, sigma);
  y ~ binomial(n, p);
}
