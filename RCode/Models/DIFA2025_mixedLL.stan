// MODIFIED STAN CODE REFLECTING WEIGHTED LIKELIHOOD
// using different sigmas depending on "official" weight

data {
  int<lower=1> n_t;
  int<lower=1> n_isos;
  array[n_isos] int<lower=0> n_dis;
  int<lower=1> mxdis;
  int<lower=1> n_haz;
  int<lower=1> n_com;

  array[n_t] real<lower=0> time;
  array[n_isos,n_t,n_com] real<lower=0> y;
  array[n_isos,n_t,n_com] real lny;

  array[n_isos, n_t, mxdis] int<lower=0, upper=1> flag;
  array[n_isos, n_t, mxdis] real<lower=0> hazdur;
  array[n_isos, mxdis] int<lower=0> htype;
  array[n_isos, mxdis, n_com] real<lower=0> iprox;

  array[n_isos,n_com] real lnmu_AR1;
  array[n_isos,n_com] real<lower=0> lnsig_AR1;
  array[n_isos,n_com] real<lower=0> lnnoise_sig;
  array[n_isos,n_t,n_com] real<lower=0, upper=1> official;
}

parameters {
  vector<lower=0>[n_haz] hsev;
  vector<lower=0>[n_com] isev;
  real beta_dis;
  real<lower=0> beta_dur;
  real<lower=0> beta_muAR1;
  real<lower=0,upper=1> beta_sigAR1;
}

model {
  vector[n_com] mu;
  vector[n_com] dsev;
  vector[n_com] sigma;
  vector[n_com] vemu_AR1;

  for (iso in 1:n_isos) {
    vemu_AR1 = beta_muAR1 * to_vector(lnmu_AR1[iso]);
    sigma = beta_sigAR1 * to_vector(lnsig_AR1[iso]);

    vector[n_dis[iso]] hs = hsev[htype[iso, 1:n_dis[iso]]];

    for (ttt in 2:n_t) {
      dsev = rep_vector(0, n_com);
      vector[n_dis[iso]] flag_vec = to_vector(flag[iso, ttt, 1:n_dis[iso]]);

      if (sum(flag_vec) > 0) {
        for (ic in 1:n_com) {
          if (sum(iprox[iso, 1:n_dis[iso], ic]) > 0) {
            vector[n_dis[iso]] iproxhs = to_vector(iprox[iso, 1:n_dis[iso], ic]) .* hs;
            dsev[ic] = log(1 + sum(flag_vec .* (rep_vector(1.0, n_dis[iso]) +
                                to_vector(hazdur[iso, ttt, 1:n_dis[iso]]) * beta_dur) .*
                                exp(iproxhs)));
          }
        }
      }

      mu = beta_dis * dsev .* isev + vemu_AR1 .* to_vector(lny[iso, ttt - 1]);

      for (ic in 1:n_com) {
        real o = official[iso, ttt, ic];
        real log_model = normal_lpdf(lny[iso, ttt, ic] | mu[ic], sigma[ic]);
        real log_fallback = normal_lpdf(lny[iso, ttt, ic] | mu[ic], lnnoise_sig[iso, ic]);
        target += log_mix(o, log_model, log_fallback); // Stable interpolation
      }
    }
  }

  hsev ~ gamma(1, 1);
  beta_dis ~ normal(0, 1);
  beta_dur ~ gamma(1, 1);
  isev ~ gamma(1, 1);
  beta_muAR1 ~ gamma(1, 1);
  beta_sigAR1 ~ beta(20, 1);
}
