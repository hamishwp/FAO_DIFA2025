// NOTES
// - if this works, then beta_dis should be negative (beta_dis is the disaster regression coefficient)
// - duration must be in years, not days
// - check conversion of array dimensions from R to stan: row-column or column-row?
// - check whether normal(mu_dis[iso, 1:n_dis[iso]], sigma_dis[iso, 1:n_dis[iso]]) needs to be iterated over instead of the vector input

data {
  // Data dimensions
  int<lower=1> n_t; // Number of years
  int<lower=1> n_isos; // Number of countries
  array[n_isos] int<lower=1> n_dis; // Number of disasters, per country
  int<lower=1> n_haz; // Number of hazard types
  int<lower=1> n_com; // Number of commodities
  // Time series data - EOY
  array[n_t] real<lower=0> time;
  // Commodity data
  array[n_isos,n_t,n_com] real<lower=0> y;
  // Flag to ensure disasters do not contribute to years previous to the disaster occurrence
  array[n_isos, n_t, max(n_dis)] int <lower = 0, upper = 1> flag;
  // Time t-1 of the disaster since the end of the hazard for EOY t
  array[n_isos, n_t, max(n_dis)] real <lower = 0> ts;
  // Time t of the disaster since the end of the hazard for EOY t
  array[n_isos, n_t, max(n_dis)] real <lower = 0> tf;
  // Duration of the disaster post-hazard during year ttt
  array[n_isos, n_t, max(n_dis)] real <lower = 0> hazdur;
  // Hazard type of the disaster
  array[n_isos, max(n_dis)] int <lower = 0> htype;
  // Expected value of disaster severity, per disaster
  array[n_isos, max(n_dis)] real<lower=0> iprox;
  // Mean AR1 trend in commodity data, per country
  array[n_isos,n_com] real mu_AR1;
  // Standard deviation in AR1 trend in commodity data, per country
  array[n_isos,n_com] real<lower=0> sig_AR1;
}

parameters {
  // Disaster parameters
  vector<lower=0>[n_haz] hsev; // Hazard severity, per hazard type
  vector[n_isos] csev; //  Country severity
  vector[n_com] isev; //  Item/Commodity severity
  real beta_dis; // Disaster-severity regression coefficient
  real<lower=0> beta_dur; // Hazard duration regression coefficient
  // GPR covariance parameters
  vector<lower=0>[n_isos] rho; // GPR length-scale
  vector<lower=0>[n_isos] alpha; // GPR marginal standard-deviation
  array[n_isos,n_com] real beta_y1; // GPR AR1 mean function trend, per country
  // vector[n_isos] beta_0; // GPR time=0 AR bias correction to mean function trend, per country
}

model {
  // Priors
  hsev ~ gamma(2,1); // Hazard severity
  csev ~ normal(0,1); // Country severity
  isev ~ normal(0,1); // Commodity severity
  rho ~ gamma(2,2); // GPR length-scale
  alpha ~ gamma(2,1); // GPR marginal standard-deviation
  beta_dis ~ normal(0,5); // Disaster-severity regression coefficient
  beta_dur ~ gamma(2,1); // Hazard duration coefficient
  // GPR mean function
  vector[n_com] mu;
  // Per country, sample from the model!
  for(iso in 1:n_isos){
    // GPR AR1 mean function coefficient - empirical Bayes
    beta_y1[iso,] ~ normal(to_vector(mu_AR1[iso,]), to_vector(sig_AR1[iso,])); 
    // GPR Covariance matrix
    matrix[n_t, n_t] K = add_diag(gp_exp_quad_cov(time, alpha[iso], rho[iso]),1e-6);
    // Decompose the GPR covariance matrix
    matrix[n_t, n_t] L_K = cholesky_decompose(K);
    // Set the GPR mean function to zero
    mu = rep_vector(0, n_com);
    // Sample through the EOY values
    for(ttt in 1:n_t){
      // Set the disaster severity to zero at first, as well as the GPR mean function
      real dsev = 0;
      // Save some computation
      vector[n_dis[iso]] flag_vec = to_vector(flag[iso, ttt, 1:n_dis[iso]]);
      // Sum all the contributing disaster components if there are any non-zero values
      if(sum(flag_vec)>0){
        // Save on computation
        vector[n_dis[iso]] iprox_vec = exp(to_vector(iprox[iso, 1:n_dis[iso]]));
        vector[n_dis[iso]] iphs = iprox_vec ./ hsev[htype[iso, 1:n_dis[iso]]];
        // Calculate the disaster severity
        dsev = sum(flag_vec.*(
          iprox_vec.*to_vector(hazdur[iso, ttt, 1:n_dis[iso]])*beta_dur +
          iprox_vec.*iphs.*(exp(-to_vector(ts[iso,ttt, 1:n_dis[iso]])./iphs)-
          exp(-to_vector(tf[iso,ttt, 1:n_dis[iso]])./iphs)
          )));
      }
      // Set the GPR mean function
      if (ttt == 1) {
        mu = to_vector(y[iso, ttt, ]);  // No past y available
      } else {
        mu = beta_dis*dsev*(1 + csev[iso] + isev) + to_vector(beta_y1[iso, ]).*to_vector(y[iso, ttt-1, ]); // Auto-Regressive first order (AR1) model
      }
      // Sample the commodity data!
      to_vector(y[iso, ttt, ]) ~ multi_normal_cholesky(mu, L_K);
    }
  }
}






