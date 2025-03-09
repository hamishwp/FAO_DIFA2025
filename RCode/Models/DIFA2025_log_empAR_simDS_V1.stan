// NOTES
// - if this works, then beta_dis should be negative (beta_dis is the disaster regression coefficient)
// - tdecay must be in years, not days
// - check conversion of array dimensions from R to stan: row-column or column-row?
// - check whether normal(mu_dis[iso, 1:n_dis[iso]], sigma_dis[iso, 1:n_dis[iso]]) needs to be iterated over instead of the vector input

data {
  // Data dimensions
  int<lower=1> n_t; // Number of years
  int<lower=1> n_isos; // Number of countries
  array[n_isos] int<lower=0> n_dis; // Number of disasters, per country
  int<lower=1> n_haz; // Number of hazard types
  int<lower=1> n_com; // Number of commodities
  // Time series data - EOY
  array[n_t] real<lower=0> time;
  // Commodity data
  array[n_isos,n_t,n_com] real<lower=0> y;
  array[n_isos,n_t,n_com] real lny; 
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
  array[n_isos, max(n_dis), n_com] real<lower=0> iprox;
  // Mean AR1 trend in commodity data, per country
  array[n_isos,n_com] real lnmu_AR1;
  // Standard deviation in AR1 trend in commodity data, per country
  array[n_isos,n_com] real<lower=0> lnsig_AR1;
  // Expected values of the commodity price data
  // array[n_isos, n_t, n_com] real mu_price;
  // Variance of the commodity price data
  // array[n_isos, n_t, n_com] real<lower=0> sig_price;
  // Weighting the likelihood by the range of magnitude of the residuals of the AR commodity predictions, per country
  // array[n_isos,n_com] real<lower = 0, upper = 1> weights;
}

parameters {
  // Disaster parameters
  vector<lower=0>[n_haz] hsev; // Hazard severity, per hazard type
  vector<lower=0>[n_com] isev; //  Item/Commodity severity
  real beta_dis; // Disaster-severity regression coefficient
  real<lower=0> beta_dur; // Hazard duration regression coefficient
  real<lower=0> beta_muAR1; // AR1 mean function trend, per country
  real<lower=0,upper=1> beta_sigAR1; // AR1 standard deviation function trend, per country
}

model {
  vector[n_com] mu; // Raw AR1 mean
  vector[n_com] dsev; // Cumulative total disaster severity 
  vector[n_com] sigma;
  vector[n_com] vemu_AR1;
  // Per country, sample from the model!
  for(iso in 1:n_isos){
    // Initialise temporary variables here to reduce computation
    vector[n_dis[iso]] flag_vec;
    vector[n_dis[iso]] iproxhs;
    vector[n_dis[iso]] hs;
    // AR1 mean (phi)
    vemu_AR1 = beta_muAR1*to_vector(lnmu_AR1[iso, ]);
    // AR1 s.d.
    sigma = beta_sigAR1*to_vector(lnsig_AR1[iso,]);
    // Save on computation
    hs = hsev[htype[iso, 1:n_dis[iso]]];
    // Sample through the EOY values
    for(ttt in 2:n_t){
      // Set the disaster severity to zero at first, as well as the AR1 mean function
      dsev = rep_vector(0,n_com);
      // Save some computation
      flag_vec = to_vector(flag[iso, ttt, 1:n_dis[iso]]);
      // Sum all the contributing disaster components if there are any non-zero values
      if(sum(flag_vec)>0){
        // Loop over commodities
        for(ic in 1:n_com){
          if(sum(iprox[iso, 1:n_dis[iso], ic])>0){
            // Save on computation  
            iproxhs = to_vector(iprox[iso, 1:n_dis[iso], ic]) ./ hs[1:n_dis[iso]];
            // Calculate the disaster severity
            dsev[ic] = sum(flag_vec.*(rep_vector(1,n_dis[iso])+to_vector(hazdur[iso, ttt, 1:n_dis[iso]])*beta_dur)*log(1+sum(exp(iproxhs))));
          }
        }
        // Set the mean function
        mu = beta_dis*dsev.*isev + vemu_AR1.*to_vector(lny[iso, ttt-1, ]); // Auto-Regressive first order (AR1) model
        // Reparameterized likelihood
        to_vector(lny[iso, ttt, ]) ~ normal(mu, sigma);
      }
    }
  }
  // Priors
  hsev ~ gamma(1,1); // Hazard severity
  beta_dis ~ normal(0,1); // Disaster-severity regression coefficient
  beta_dur ~ gamma(1,1); // Hazard duration coefficient
  isev ~ gamma(1,1); // Commodity severity
  beta_muAR1 ~ gamma(1,1);
  beta_sigAR1 ~ beta(20,1);
}


// generated quantities {
//   // Declare price arrays
//   array[n_isos, n_t, n_com] real price_dis = rep_array(0, n_isos, n_t, n_com);
//   array[n_isos, n_t, n_com] real price_nodis = rep_array(0, n_isos, n_t, n_com);
//   array[n_isos, n_t, n_com] real price_diff = rep_array(0, n_isos, n_t, n_com);
//   array[n_isos, n_t, n_com] real y_dis = y;
//   array[n_isos, n_t, n_com] real y_nodis = y;
//   // Iterate over countries, years, and commodities
//   for (iso in 1:n_isos) {
//     vector[n_com] dsev; // Disaster severity per commodity
//     // Iterate over years
//     for (ttt in 2:n_t) {
//       // Set the disaster severity to zero at first, as well as the AR1 mean function
//       dsev = rep_vector(0,n_com);
//       // Save some computation
//       flag_vec = to_vector(flag[iso, ttt, 1:n_dis[iso]]);
//       // If any disasters occurred
//       if (sum(flag_vec) > 0) {
//         for (ic in 1:n_com) {
//           // Save on computation
//           vector[n_dis[iso]] iprox_vec = exp(to_vector(iprox[iso, 1:n_dis[iso], ic]));
//           vector[n_dis[iso]] iphs = iprox_vec ./ hsev[htype[iso, 1:n_dis[iso]]];
//           // Calculate the disaster severity
//           dsev[ic] = sum(flag_vec.*(
//             iphs.*to_vector(hazdur[iso, ttt, 1:n_dis[iso]])*beta_dur +
//             iprox_vec.*iphs.*(exp(-to_vector(ts[iso,ttt, 1:n_dis[iso]])./iphs)-
//             exp(-to_vector(tf[iso,ttt, 1:n_dis[iso]])./iphs))));
//             // Set the AR1 mean function
//             real mu_dis = beta_dis*dsev[ic].*(1 + isev[ic]) + beta_y1[iso, ic].*y[iso, ttt-1, ic]; // Auto-Regressive first order (AR1) model
//             real mu_nodis = beta_y1[iso, ic].*y[iso, ttt-1, ic]; // Auto-Regressive first order (AR1) model
//             real red_sig = sig_AR1[iso, ic] * sigma[iso, ic]; // Adjusted AR1 standard deviation
//             // Compute production
//             y_dis[iso, ttt, ic] = normal_rng(mu_disaster, red_sig);
//             y_nodis[iso, ttt, ic] = normal_rng(mu_no_disaster, red_sig);
//             // Compute expected prices with disasters (actual beta_dis)
//             price_dis[iso, ttt, ic] = normal_rng(mu_price[iso, ic] * y_dis[iso, ttt, ic], sig_price[iso, ic]);
//             price_nodis[iso, ttt, ic] = normal_rng(mu_price[iso, ic] * y_nodis[iso, ttt, ic], sig_price[iso, ic]);
//             // Compute price difference
//             price_diff[iso, ttt, ic] = price_dis[iso, ttt, ic] - price_nodis[iso, ttt, ic];
//         }
//       }
//     }
//   }
// }







