set.seed(44)  # Ensure reproducibility

# ----- Define Data Dimensions -----
n_t <- 20        # Number of years
n_isos <- 10      # Number of countries
n_haz <- 3       # Number of hazard types

# Each country has a random number of disasters (between 1 and 5)
n_dis <- sample(1:25, n_isos, replace = TRUE) 

# Time series (assumed to be evenly spaced years)
time <- seq(1, n_t)

# ----- Define Parameters -----
# Gaussian Process Covariance Parameters
rho <- rgamma(n_isos, shape = 2, scale = 2)   # Length-scale
alpha <- rgamma(n_isos, shape = 2, scale = 1) # Marginal standard deviation
# sigma <- rgamma(n_isos, shape = 2, scale = 1) # Noise scale
# AR1 Mean Function Priors
mu_AR1 <- rep(1.05,n_isos)# rnorm(n_isos, mean = 1.2, sd = 0.1)       # Mean AR1 trend per country
sig_AR1 <- rep(0.1,n_isos)#runif(n_isos, 0.01, 0.05)                # Standard deviation of AR1 trend
beta_y1 <- rnorm(n_isos, mu_AR1, sig_AR1)   # AR1 mean function trend
beta_0 <- abs(rnorm(n_isos, mean = 1000, sd = 300))       # Initial bias correction
# Disaster Parameters
hsev <- (1:n_haz)^2 # rgamma(n_haz, shape = 2, scale = 1)  # Hazard severity by type
csev <- rnorm(n_isos, mean = 0, sd = 0.25) # rep(0.,n_isos)  # Country severity
beta_dis <- -5 # Disaster-severity regression coefficient
beta_dur <- 1. # Hazard duration contribution to disaster severity
alpha_d<-2
lambda_d<-0.5

# ----- Generate Disaster Information -----
# Hazard types per disaster 
htype <- array(0, dim = c(n_isos, max(n_dis))) 
# Disaster occurrence flag
flag <- array(0, dim = c(n_isos, n_t, max(n_dis))) 
# Hazard duration per year
hazdur <- array(0, dim = c(n_isos, n_t, max(n_dis))) 
# Disaster duration start to end window per year
ts <- array(0, dim = c(n_isos, n_t, max(n_dis))) 
tf <- array(0, dim = c(n_isos, n_t, max(n_dis))) 
# Disaster severity
iprox <- array(0, dim = c(n_isos, max(n_dis))) 

# Generate disasters for each country
for (iso in 1:n_isos) {
  # Which year did the disaster occur?
  dis_years <- sample(1:n_t, n_dis[iso], replace = TRUE)  # Ensure enough space for multi-year disasters
  # Which hazard type was the disaster (nominal data)
  htype[iso,] <- sample(1:n_haz, max(n_dis), replace = TRUE)
  # Generate the disaster severity
  iprox[iso, ] <- rgamma(max(n_dis), shape = alpha_d, scale = lambda_d)  # Random disaster severity
  # Estimate how long each disaster has an impact on the countries production
  for (i in 1:n_dis[iso]) {
    start_year <- dis_years[i]
    duration_years <- rgamma(1,0.3,1)*htype[iso,i]  # Ensure enough space for multi-year disasters
    startprop <- runif(1,0,1) # At which proportion of the year did the disaster occur?
    end_year <- min(floor(start_year + duration_years), n_t)  # Ensure disaster doesn't exceed time range
    endt <- start_year+startprop+duration_years
    # Set the flag for the disaster impact on EOY values
    flag[iso, start_year:n_t, i] <- 1  # Mark disaster occurrence
    hazdur[iso, start_year, i] <- duration_years
    # Calculate the hazard and disaster duration arrays
    for (t in start_year:n_t) {
      if (t == start_year) {
        if(1-startprop>=duration_years){ # hazard lasts less than one year
          ts[iso, t, i] <- 0.
          tf[iso, t, i] <- 1-startprop-duration_years
        } else { # hazard lasts more than in the initial year
          ts[iso, t, i] <- 1
          tf[iso, t, i] <- 1
        }
      } else if (t < endt){        
        # Intermediate years: Hazard is active for the full year, no additional duration needed
        ts[iso, t, i] <- 1
        tf[iso, t, i] <- 1
      } else if (t > endt & t < endt + 1){
        # Final year: Hazard ends sometime in the year
        ts[iso, t, i] <- 0.
        tf[iso, t, i] <- t-endt
      } else {
        ts[iso, t, i] <- t-1-endt
        tf[iso, t, i] <- t-endt
      }
    }
  }
}

# Quick check
if(any(ts<0) | any(tf<0)) stop("Issues with disaster decay time start and end values")

# ----- Generate GP for Each Country -----
y <- array(0, dim = c(n_isos, n_t))  # Commodity data (final output)
mu <- array(0, dim = c(n_isos, n_t)) # Mean function (GP + AR1)

for (iso in 1:n_isos) {
  # Construct Covariance Matrix (Squared Exponential Kernel)
  K <- outer(time, time, function(t1, t2) alpha[iso]^2 * exp(-0.5 * (t1 - t2)^2 / rho[iso]^2))
  # diag(K) <- diag(K) + sigma[iso]^2  # Add noise
  
  # Sample GP from Multivariate Normal
  gp_samples <- MASS::mvrnorm(n = 1, mu = rep(0, n_t), Sigma = K)
  
  # Compute disaster severity effect over time
  for (t in 1:n_t) {
    dsev <- 0
    if(sum(flag[iso,t,])>0){
      # Save on computation
      iphs = iprox[iso,1:n_dis[iso]] / hsev[htype[iso, 1:n_dis[iso]]]
      # Calculate the disaster severity
      dsev = sum(flag[iso,t,1:n_dis[iso]]*(
        iprox[iso,1:n_dis[iso]]*hazdur[iso, t, 1:n_dis[iso]]*beta_dur +
          iprox[iso,1:n_dis[iso]]*iphs*(exp(-ts[iso,t, 1:n_dis[iso]]/iphs)-
                                          exp(-tf[iso,t, 1:n_dis[iso]]/iphs))
      ))
    }
    # Compute Mean Function (GP + AR1)
    if (t == 1) {
      mu[iso, t] <- beta_0[iso]
    } else {
      mu[iso, t] <- beta_dis * dsev * (1 + csev[iso]) + beta_y1[iso] * y[iso, t-1]
    }
    # Generate final commodity data from GP + AR1 model
    y[iso, t] <- mu[iso, t] + gp_samples[t]
  }
  
}

y

# ----- Format Data for Stan -----
data_list <- list(
  n_t = n_t,
  n_isos = n_isos,
  n_dis = n_dis,
  n_haz = n_haz,
  time = time,
  y = y,  # Transposed for Stan (column-major order)
  flag = flag,
  ts = ts,
  tf = tf,
  hazdur = hazdur,
  htype = htype,
  iprox = log(iprox),
  mu_AR1 = mu_AR1,
  sig_AR1 = sig_AR1
)

     
library(rstan)

# ----- Step 0: Prepare Stan Data -----
# Assuming data_list from the previous script is already generated
# Ensure that Stan runs properly with parallel computation enabled
options(mc.cores = floor(0.8*parallel::detectCores()))
rstan_options(auto_write = TRUE)

# ----- Compile Stan Model -----
# stan_model_code <- "./RCode/Models/DIFA2025_redredDisSev.stan"  # Specify your Stan model path
stan_model_code <- "./RCode/Models/DIFA2025_redDisSev.stan"  # Specify your Stan model path
if(grepl("DIFA2025.stan",stan_model_code) | grepl("DIFA2025_csev.stan",stan_model_code)) {
  data_list$mu_dis<-array(2,dim = c(n_isos,max(n_dis)))
  data_list$sig_dis<-array(0.1,dim = c(n_isos,max(n_dis)))
}
  
stan_model <- stan_model(stan_model_code)

# ----- MCMC Sampling -----
mcmc_results <- sampling(
  object = stan_model, 
  data = data_list, 
  chains = 8, 
  iter = 3500, 
  # thin = 4,
  warmup = 1500, 
  seed = 42,
  control = list(adapt_delta = 0.95, max_treedepth=15),
  sample_file="./Data/Results/Simulations/DIFA2025_csevOnly.csv"
)
print(mcmc_results)

# Plot MCMC diagnostics
traceplot(mcmc_results, pars = c("beta_dis", "hsev", "beta_y1"))

mcmc_results<-rstan::read_stan_csv(list.files("./Data/Results/Simulations/RedRedDis_Success/",full.names = T))


