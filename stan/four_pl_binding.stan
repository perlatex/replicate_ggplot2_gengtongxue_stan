functions {
  real four_pl(real x, real bottom, real top, real Kd, real hill) {
    return bottom + (top - bottom) / (1 + pow(x / Kd, hill));
  }
}

data {
  int<lower=1> N;
  vector<lower=0>[N] x;
  vector[N] y;

  int<lower=1> N_new;
  vector<lower=0>[N_new] x_new;
}



parameters {
  real<lower=0> top;      // 上平台
  real<lower=0> bottom;   // 下平台
  real<lower=0> Kd;       // 半最大效应浓度
  real<lower=0> hill;     // 曲线陡峭程度
  real<lower=0> sigma;    // 观测误差
}

transformed parameters {
  vector[N] mu;

  for (i in 1:N) {
    mu[i] = four_pl(x[i], bottom, top, Kd, hill);
  }
}

model {
  // priors
  top    ~ normal(20000, 5000);
  bottom ~ normal(0, 2000);
  Kd     ~ lognormal(log(2), 1);
  hill   ~ lognormal(0, 0.5);
  sigma  ~ exponential(1.0 / 2000);

  // likelihood
  y ~ normal(mu, sigma);
}

generated quantities {
  vector[N] y_rep;
  vector[N] log_lik;
  vector[N_new] mu_new;
  
  for (i in 1:N) {
    y_rep[i] = normal_rng(mu[i], sigma);
    log_lik[i] = normal_lpdf(y[i] | mu[i], sigma);
  }
  
  for (i in 1:N_new) {
    mu_new[i] =
      bottom + (top - bottom) /
      (1 + pow(x_new[i] / Kd, hill));
  }
  
}

