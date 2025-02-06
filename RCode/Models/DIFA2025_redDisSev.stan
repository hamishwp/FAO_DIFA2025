// NOTES
// - if this works, then betad should be negative (betad is the disaster regression coefficient)
// - duration must be in years, not days
// - check conversion of array dimensions from R to stan: row-column or column-row?
// - check whether normal(mu_dis[iso, 1:n_dis[iso]], sigma_dis[iso, 1:n_dis[iso]]) needs to be iterated over instead of the vector input

data {
  // Data dimensions
  int<lower=1> n_t; // Number of years
  int<lower=1> n_isos; // Number of countries
  int<lower=1> n_dis[n_isos]; // Number of disasters, per country
  int<lower=1> n_haz; // Number of hazard types
  // Time series data - EOY
  vector[n_t] real time;
  // Commodity data
  matrix[n_isos,n_t] real y;
  // Flag to ensure disasters do not contribute to years previous to the disaster occurrence
  array[n_isos, n_t, max(n_dis)] int <lower = 0, upper = 1> flag;
  // Duration of the hazard during year ttt
  array[n_isos, n_t, max(n_dis)] real <lower = 0> duration;
  // Hazard type of the disaster
  array[max(n_dis)] int <lower = 0> htype;
  // Expected value of disaster severity, per disaster
  array[n_isos, max(n_dis)] real<lower=0> iprox;
  // Mean linear trend in commodity data, per country
  vector[n_isos] real mu_lin;
  // Standard deviation in linear trend in commodity data, per country
  vector[n_isos] real<lower=0> sigma_lin;
}

parameters {
  // Disaster parameters
  vector<lower=0>[n_haz] hsev; // Hazard severity, per hazard type
  vector[n_isos] csev; //  Country severity
  real betad; // Disaster-severity regression coefficient
  // GPR covariance parameters
  vector<lower=0>[n_isos] rho; // GPR length-scale
  vector<lower=0>[n_isos] alpha; // GPR marginal standard-deviation
  vector<lower=0>[n_isos] sigma; // GPR regression-level noise scale
  vector[n_isos] lin; // GPR linear mean function trend, per country
}

transformed parameters {
  vector<lower=0>[n_isos] sq_sigma;
  sq_sigma = square(sigma[1:n_isos]);
}

model {
 // Priors
 hsev ~ gamma(2,1); // Hazard severity
 csev ~ normal(0,1); // Country severity
 rho ~ gamma(2,2); // GPR length-scale
 alpha ~ gamma(2,1); // GPR marginal standard-deviation
 sigma ~ gamma(2,1); // GPR regression-level noise scale
 betad ~ normal(0,5); // Disaster-severity regression coefficient
 lin ~ normal(mu_lin, 2*sig_lin); // GPR linear mean function coefficient - empirical Bayes
 // GPR mean function
 vector[n_t] mu;
 // Per country, sample from the model!
 for(iso in 1:n_isos){
   // GPR Covariance matrix
   matrix[n_t, n_t] K = gp_exp_quad_cov(time, alpha[iso], rho[iso]) + 
                        diag_matrix(rep_vector(sq_sigma[iso], n_t));
   // Decompose the GPR covariance matrix
   matrix[n_t, n_t] L_K = cholesky_decompose(K);
   // Set the GPR mean function to zero
   mu = rep_vector(0, n_t);
   // Sample through the EOY values
   for(ttt in 1:n_t){
     // Set the disaster severity to zero at first, as well as the GPR mean function
     real<lower=0> dsev = 0;
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
     mu[ttt] = betad * dsev * (1 + csev[iso]) + lin[iso] * time[ttt];;
   }
   // Sample the commodity data!
   y[iso,] ~ multi_normal_cholesky(mu, L_K);
 }
}