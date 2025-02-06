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
sigma <- rgamma(n_isos, shape = 2, scale = 1) # Noise scale
# AR1 Mean Function Priors
mu_AR1 <- rnorm(n_isos, mean = 1.2, sd = 0.1)       # Mean AR1 trend per country
sig_AR1 <- runif(n_isos, 0.01, 0.05)                # Standard deviation of AR1 trend
beta_y1 <- rnorm(n_isos, mu_AR1, sig_AR1)   # AR1 mean function trend
beta_0 <- abs(rnorm(n_isos, mean = 1000, sd = 300))       # Initial bias correction
# Disaster Parameters
hsev <- rgamma(n_haz, shape = 2, scale = 1)  # Hazard severity by type
csev <- rnorm(n_isos, mean = 0, sd = 0.5)      # Country severity
beta_dis <- -40                                # Disaster-severity regression coefficient

# ----- Generate Disaster Information -----
# Hazard types per disaster 
htype <- array(0, dim = c(n_isos, max(n_dis))) 
# Disaster occurrence flag
flag <- array(0, dim = c(n_isos, n_t, max(n_dis))) 
# Disaster duration per year
duration <- array(0, dim = c(n_isos, n_t, max(n_dis))) 
# Disaster severity
iprox <- matrix(0, nrow = n_isos, ncol = max(n_dis))

# Generate disasters for each country
for (iso in 1:n_isos) {
  # Which year did the disaster occur?
  dis_years <- sample(1:(n_t - 3), n_dis[iso], replace = TRUE)  # Ensure enough space for multi-year disasters
  # Which hazard type was the disaster (nominal data)
  htype[iso,] <- sample(1:n_haz, max(n_dis), replace = TRUE)
  # Generate the disaster severity
  iprox[iso, ] <- rgamma(max(n_dis), shape = 2, scale = 1)  # Random disaster severity
  # Estimate how long each disaster has an impact on the countries production
  for (i in 1:n_dis[iso]) {
    start_year <- dis_years[i]
    duration_years <- rgamma(1,0.08,1)*htype[iso,i]  # Ensure enough space for multi-year disasters
    end_year <- min(start_year + duration_years, n_t)  # Ensure disaster doesn't exceed time range
    # Set the flag for the disaster impact on EOY values
    flag[iso, start_year:n_t, i] <- 1  # Mark disaster occurrence
    for (t in start_year:end_year) {
      if (t == start_year | t == end_year) {
        duration[iso, t, i] <- runif(1, 0, 1)  # First & last year: fraction of year
      } else {
        duration[iso, t, i] <- 1  # Full year duration for intermediate years
      }
    }
  }
}

# ----- Generate GP for Each Country -----
y <- matrix(0, nrow = n_isos, ncol = n_t)  # Commodity data (final output)
mu <- matrix(0, nrow = n_isos, ncol = n_t) # Mean function (GP + AR1)

for (iso in 1:n_isos) {
  # Construct Covariance Matrix (Squared Exponential Kernel)
  K <- outer(time, time, function(t1, t2) alpha[iso]^2 * exp(-0.5 * (t1 - t2)^2 / rho[iso]^2))
  diag(K) <- diag(K) + sigma[iso]^2  # Add noise
  
  # Sample GP from Multivariate Normal
  gp_samples <- MASS::mvrnorm(n = 1, mu = rep(0, n_t), Sigma = K)
  
  # Compute disaster severity effect over time
  for (t in 1:n_t) {
    dsev <- 0
    for (i_dis in 1:n_dis[iso]) {
      if (flag[iso, t, i_dis] == 1) {
        iphs <- iprox[iso, i_dis] / hsev[htype[i_dis]]
        dsev <- dsev + iprox[iso, i_dis] * (duration[iso, t, i_dis] + 
                                              iphs * (1 - exp(-duration[iso, t, i_dis] / iphs)))
      }
    }
    
    # Compute Mean Function (GP + AR1)
    if (t == 1) {
      mu[iso, t] <- beta_dis * dsev * (1 + csev[iso]) + beta_0[iso]
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
  y = t(y),  # Transposed for Stan (column-major order)
  flag = flag,
  duration = duration,
  htype = htype,
  iprox = iprox,
  mu_AR1 = mu_AR1,
  sig_AR1 = sig_AR1
)

# Print dataset structure to verify
str(data_list)
                        







































