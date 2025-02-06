// NOTES
// - if this works, then beta_dis should be negative (beta_dis is the disaster regression coefficient)
// - duration must be in years, not days
// - check conversion of array dimensions from R to stan: row-column or column-row?
// - check whether normal(mu_dis[iso, 1:n_dis[iso]], sigma_dis[iso, 1:n_dis[iso]]) needs to be iterated over instead of the vector input
// - alpha_dis and lambda_dis must have 0 and near 0 values, resp, for all array elements that do not have disasters in the iprox[n_isos,n_dis] array

data {
  // Data dimensions
  int<lower=1> n_t; // Number of years
  int<lower=1> n_isos; // Number of countries
  array[n_isos] int<lower=1> n_dis; // Number of disasters, per country
  int<lower=1> n_haz; // Number of hazard types
  // Time series data - EOY
  array[n_t] real time;
  // Commodity data
  matrix[n_isos,n_t] y;
  // Flag to ensure disasters do not contribute to years previous to the disaster occurrence
  array[n_isos, n_t, max(n_dis)] int <lower = 0, upper = 1> flag;
  // Duration of the hazard during year ttt
  array[n_isos, n_t, max(n_dis)] real <lower = 0> duration;
  // Hazard type of the disaster
  array[max(n_dis)] int <lower = 0> htype;
  // (Gamma) Shape value of disaster severity, per disaster
  array[n_isos, n_dis] real<lower=0> alpha_dis;
  // (Gamma) Rate of disaster severity, per disaster
  array[n_isos, n_dis] real<lower=0> lambda_dis;
  // Mean AR1 trend in commodity data, per country
  vector[n_isos] mu_AR1;
  // Standard deviation in AR1 trend in commodity data, per country
  array[n_isos] real<lower=0> sig_AR1;
}

parameters {
  // Disaster parameters
  vector<lower=0>[n_haz] hsev; // Hazard severity, per hazard type
  vector[n_isos] csev; //  Country severity
  real beta_dis; // Disaster-severity regression coefficient
  array[n_isos] vector<lower=0>[max(n_dis)] iprox; // Disaster-specific severity
  // GPR covariance parameters
  vector<lower=0>[n_isos] rho; // GPR length-scale
  vector<lower=0>[n_isos] alpha; // GPR marginal standard-deviation
  vector<lower=0>[n_isos] sigma; // GPR regression-level noise scale
  vector[n_isos] beta_y1; // GPR AR1 mean function trend, per country
  vector[n_isos] beta_0; // GPR time=0 AR bias correction to mean function trend, per country
}

transformed parameters {
  vector<lower=0>[n_isos] sq_sigma;
  sq_sigma = square(sigma[1:n_isos]);
  real<lower=0> dsev;
}

model {
 // Priors
 hsev ~ gamma(2,1); // Hazard severity
 csev ~ normal(0,1); // Country severity
 rho ~ gamma(2,2); // GPR length-scale
 alpha ~ gamma(2,1); // GPR marginal standard-deviation
 sigma ~ gamma(2,1); // GPR regression-level noise scale
 beta_dis ~ normal(0,5); // Disaster-severity regression coefficient
 beta_y1 ~ normal(mu_AR1, 3*sig_AR1); // GPR AR1 mean function coefficient - empirical Bayes
 beta_0 ~ normal(0,5); // GPR time=0 regression bias correction
 // GPR mean function
 vector[n_t] mu;
 // Per country, sample from the model!
 for(iso in 1:n_isos){
   // GPR Covariance matrix
   matrix[n_t, n_t] K = add_diag(gp_exp_quad_cov(time, alpha[iso], rho[iso]), sq_sigma[iso]);
   // Decompose the GPR covariance matrix
   matrix[n_t, n_t] L_K = cholesky_decompose(K);
   // Set the GPR mean function to zero
   mu = rep_vector(0, n_t);
   // Sample the estimated impact value in crop losses = impact proxy
   iprox[iso,] ~ gamma(alpha_dis[iso, 1:max(n_dis)], lambda_dis[iso, 1:max(n_dis)]);
   // Sample through the EOY values
   for(ttt in 1:n_t){
     // Set the disaster severity to zero at first, as well as the GPR mean function
     dsev = 0;
     // Sample the disaster impact type on crops and cattle losses
     for(i_dis in 1:n_dis[iso]){
       // Check if the disaster comes after or before this year.
       if(flag[iso,ttt,i_dis]!=0) {
         // Save on computation
         real iphs = iprox[iso,i_dis] / hsev[htype[i_dis]]; 
         // Calculate the EOY disaster severity based on this disaster and add to total disaster severity for this EOY
         dsev += iprox[iso,i_dis] * (duration[iso, ttt, i_dis] + iphs *(1-exp(-duration[iso, ttt, i_dis]/iphs)));
       } 
     }
     // Set the GPR mean function
     if (ttt == 1) {
        mu[ttt] = beta_dis*dsev*(1 + csev[iso]) + beta_0;  // No past y available
      } else {
        mu[ttt] = beta_dis*dsev*(1 + csev[iso]) + beta_y1*y[iso, ttt-1]; // Auto-Regressive first order (AR1) model
      }
   }
   // Sample the commodity data!
   y[iso,] ~ multi_normal_cholesky(mu, L_K);
 }
}