set.seed(44)  # Ensure reproducibility

Dataset <- GetModelProduction()

Item <-Dataset%>%
  filter(
    item_grouping_2 == "Plant_based"
  )

Item_wide <- Item%>%
  arrange(
    ISO3.CODE,
    Year
    )%>%
  pivot_wider(
    id_cols = "ISO3.CODE",
    names_from = "Year",
    values_from = "Production"
  )

y_model <- as.matrix(
  Item_wide[-1]
)

rownames(y_model) <- Item_wide$ISO3.CODE

EMDAT <- GET_EMDAT_MODEL()

ts <- EMDAT$ts
tf <- EMDAT$tf
hazdur <- EMDAT$hazdur
flag <- EMDAT$flag

# ----- Define Data Dimensions -----
n_t <- length(unique(Item$Year)) # Number of years
n_isos <- length(unique(Item$ISO3.CODE)) # Number of countries
n_haz <- 4   # Number of hazard types
n_dis <- sample(1:25, n_isos, replace = TRUE) 
time <- seq(1, n_t)
# ----- Define Parameters -----
# Gaussian Process Covariance Parameters
rho <- rgamma(n_isos, shape = 2, scale = 2)   # Length-scale
alpha <- rgamma(n_isos, shape = 2, scale = 1) # Marginal standard deviation
# sigma <- rgamma(n_isos, shape = 2, scale = 1) # Noise scale
# AR1 Mean Function Priors
mu_AR1 <- rep(1.2,n_isos)# rnorm(n_isos, mean = 1.2, sd = 0.1)       # Mean AR1 trend per country
sig_AR1 <- rep(0.1,n_isos)#runif(n_isos, 0.01, 0.05)                # Standard deviation of AR1 trend
beta_y1 <- rnorm(n_isos, mu_AR1, sig_AR1)   # AR1 mean function trend
beta_0 <- abs(rnorm(n_isos, mean = 1000, sd = 300))       # Initial bias correction
# Disaster Parameters
hsev <- (1:n_haz)^2 # rgamma(n_haz, shape = 2, scale = 1)  # Hazard severity by type
csev <- rep(0.,n_isos)#rnorm(n_isos, mean = 0, sd = 0.5)      # Country severity
beta_dis <- -0.000001 # Disaster-severity regression coefficient
beta_dur <- 0.0001 # Hazard duration contribution to disaster severity

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
    for (i_dis in 1:n_dis[iso]) {
      if (flag[iso, t, i_dis] == 1) {
        dsev <- dsev + iprox[iso, i_dis] * hazdur[iso, t, i_dis]*beta_dur
        iphs <- iprox[iso, i_dis] / hsev[htype[i_dis]]
        if(tdecay[iso, t, i_dis]<t-1){
          # Calculate the EOY disaster severity based on this disaster and add to total disaster severity for this EOY
          dsev = dsev + iprox[iso, i_dis]*iphs*(1-exp(-(t-tdecay[iso, t, i_dis])/iphs))
        } else {
          t0 = t-1-tdecay[iso, t, i_dis]
          # Calculate the EOY disaster severity based on this disaster and add to total disaster severity for this EOY
          dsev = dsev + iprox[iso, i_dis]*iphs*(exp(-t0/iphs)-exp(-(t0+1)/iphs))
        }
      }
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
  y = y_model,  # Transposed for Stan (column-major order)
  flag = flag,
  tdecay = tdecay,
  hazdur = hazdur,
  htype = htype,
  iprox = iprox,
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
stan_model_code <- "./RCode/Models/DIFA2025_redredDisSev.stan"  # Specify your Stan model path
stan_model <- stan_model(stan_model_code)

# ----- MCMC Sampling -----
mcmc_results <- sampling(
  object = stan_model, 
  data = data_list, 
  chains = 4, 
  iter = 2000, 
  # thin = 4,
  warmup = 1000, 
  seed = 42,
  control = list(adapt_delta = 0.95, max_treedepth=15),
  sample_file="./Data/Results/Simulations/DIFA2025_redredDisSev.csv"
)
print(mcmc_results)

# Plot MCMC diagnostics
traceplot(mcmc_results, pars = c("beta_dis", "hsev", "beta_y1"))




