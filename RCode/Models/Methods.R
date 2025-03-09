TrainModel<-function(fdf,model,method="MCMC"){
  # Train the model
  if(method=="MCMC") {
    mGPR<-TrainModel_MCMC(fdf=fdf,model=stan_model_code)
  } else if(method=="VI"){
    mGPR<-TrainModel_VI(fdf=fdf,model=stan_model_code)
  } else if(method=="MCMLE"){
    mGPR<-TrainModel_MCMLE(fdf=fdf)
  } else {
    mGPR<-TrainModel_Optim(fdf=fdf,model=stan_model_code)
  }
  return(mGPR)
}

# Run the stan model
TrainModel_MCMC<-function(fdf,model){
  # Ensure that Stan runs properly with parallel computation enabled
  rstan::rstan_options(auto_write = TRUE)
  # Compile the stan code
  stan_model <- rstan::stan_model(model)
  # Initialisations
  inits_f<-InitParams(fdf,iprox_dat = iprox_dat,GPR = GPR,empAR = empAR)
  # MCMC Sampling 
  mcmc_results <- rstan::sampling(
    object = stan_model, 
    data = fdf, 
    chains = hyppars$chains, 
    iter = hyppars$iter, 
    init = inits_f,
    warmup = hyppars$burnin, 
    seed = 42,
    control = list(adapt_delta = hyppars$adapt, max_treedepth=hyppars$maxtree),
    sample_file=paste0("./Data/Results/",str_split(str_split(model,"/")[[1]][4],".stan")[[1]][1],save_str,".csv")
  )
  
  return(mcmc_results)
}

# Instead of MCMC use optimisation
TrainModel_Optim <- function(fdf, model) {
  # Ensure that Stan runs properly with parallel computation enabled
  rstan::rstan_options(auto_write = TRUE)
  # Compile the stan code
  stan_model <- rstan::stan_model(model)
  # Initializations (may not be needed for optimization)
  inits_f <- InitParams(fdf, iprox_dat = iprox_dat, GPR = GPR, empAR = empAR)
  # Run Stan optimization instead of MCMC sampling
  optim_results <- rstan::optimizing(
    object = stan_model,
    data = fdf,
    init = inits_f,
    seed = 42,
    hessian = TRUE)
  
  return(optim_results)
}

# Variational Inference
TrainModel_VI <- function(fdf, model) {
  # Ensure that Stan runs properly with parallel computation enabled
  rstan::rstan_options(auto_write = TRUE)
  # Compile the Stan model
  stan_model <- rstan::stan_model(model)
  # Initializations (optional, depends on the model)
  inits_f <- InitParams(fdf, iprox_dat = iprox_dat, GPR = GPR, empAR = empAR)
  # Variational Inference (VI) instead of MCMC sampling
  vi_results <- rstan::vb(
    object = stan_model,
    data = fdf,
    init = inits_f,
    seed = 42,
    output_samples = 1500)
  
  return(vi_results)
}

# Reduce the fdf datalist to only the variables we need 
redFdF <- function(fdf) {
  # Identify all `fdf$` elements used in the function
  fdf_vars <- c("n_isos", "n_t", "n_dis", "n_com", "n_haz", "dsev",
                "flag", "htype", "iprox", "hazdur", "ts", "tf", 
                "lnmu_AR1", "lnsig_AR1", "mu_AR1", "sig_AR1", 
                "y", "lny", "area")
  # Keep only the required elements in fdf
  fdf[names(fdf) %in% fdf_vars]
}

TrainModel_MCMLE <- function(fdf, samp = 5000, cpus = 30, LL="m_likelihood", mxdis = 30){
  # Reduce memory of fdf list
  fdf%<>%redFdF()
  fdf$n_dis<-pmin(mxdis,fdf$n_dis)
  
  # Generate parameter sets
  params_list <- lapply(1:samp, function(i) list(
    hsev = rgamma(fdf$n_haz, shape = 1, rate = 1),  # Hazard severity (vector)
    beta_dis = rnorm(1, mean = 0, sd = 3),  # Disaster-severity regression coefficient
    beta_dur = rgamma(1, shape = 1, rate = 1),  # Disaster-severity regression coefficient
    isev = rgamma(fdf$n_com, shape = 1, rate = 1),  # Commodity severity (vector)
    sigma = rbeta(1,20,1),  # Standard deviation in AR1 model
    beta_y1 = rgamma(1, shape = 2, rate = 2)  # Gamma coefficient in AR1 model
  ))
  
  # Run parallel computation
  log_likelihoods <- parallel::mclapply(params_list, function(params) {
    params$LL <- do.call(LL,list(fdf=fdf, params=params))  # Compute log-likelihood
    return(params)
  }, mc.cores = cpus)
  
  # Convert list of lists to a long data frame
  bind_rows(lapply(log_likelihoods, function(params) {
    # Convert vector elements into a tibble for long-format storage
    tibble(
      beta_dis = params$beta_dis,
      beta_dur = params$beta_dur,
      sigma = params$sigma,
      beta_y1 = params$beta_y1,
      LL = params$LL,
    ) %>%
      # Unnest vector parameters
      mutate(hsev = list(params$hsev),
             isev = list(params$isev))
  }))
}

# Restructuring the MCMLE results into one dataframe and then calculating summaries of the different parameter samples
compute_weighted_stats <- function(mcmle) {
  # Ensure LL exists in dataframe
  if (!"LL" %in% colnames(mcmle)) {
    stop("Column 'LL' not found in the dataframe.")
  }
  
  # **1. Compute statistics for all numeric columns**
  numeric_vars <- colnames(mcmle)[sapply(mcmle, is.numeric) & 
                                    colnames(mcmle) != "LL"]
  
  # Filter high LL values (upper quartile)
  mcmle_filtered <- mcmle %>%
    filter(LL > quantile(LL, 0.75)) %>%  # Keep top 25% of samples
    mutate(
      redLL = exp(LL - max(LL)),  # Log-scale LL
      weights = redLL / sum(redLL)  # Normalize weights
    )
  
  results <- do.call(rbind,lapply(numeric_vars, function(var) {
    w_mean <- weighted.mean(mcmle_filtered[[var]], mcmle_filtered$weights)
    w_quartiles <- weighted_quantile(mcmle_filtered[[var]], mcmle_filtered$weights, c(0.05, 0.25, 0.5, 0.75, 0.95))
    
    data.frame(
      variable = var,
      w_mean = w_mean,
      w_q05 = w_quartiles[1],
      w_q25 = w_quartiles[2],
      w_q50 = w_quartiles[3],
      w_q75 = w_quartiles[4],
      w_q95 = w_quartiles[5]
    )
  }))
  
  # **2. Handle `hsev` and `isev` separately (long format)**
  # **2. Expand `hsev` and `isev` into separate variables**
  # Extract max length of hsev and isev (assuming all rows have the same length)
  max_hsev_length <- max(sapply(mcmle$hsev, length))
  max_isev_length <- max(sapply(mcmle$isev, length))
  
  # Create named columns for hsev[i] and isev[i]
  mcmle_expanded <- mcmle_filtered %>%
    mutate(hsev = lapply(hsev, function(x) { length(x) <- max_hsev_length; x }),  # Pad missing elements with NA
           isev = lapply(isev, function(x) { length(x) <- max_isev_length; x })) %>%
    unnest_wider(hsev, names_sep = "_") %>%
    unnest_wider(isev, names_sep = "_")
  
  # Compute weighted statistics for each hsev[i] and isev[i]
  hsev_isev_results <- lapply(names(mcmle_expanded)[grepl("^hsev_|^isev_", names(mcmle_expanded))], function(var) {
    w_mean <- weighted.mean(mcmle_expanded[[var]], mcmle_expanded$weights, na.rm = TRUE)
    w_quartiles <- weighted_quantile(mcmle_expanded[[var]], mcmle_expanded$weights, c(0.05, 0.25, 0.5, 0.75, 0.95))
    
    data.frame(
      variable = gsub("_", "[", var) %>% gsub("$", "]", .),  # Convert hsev_1 to hsev[1]
      w_mean = w_mean,
      w_q05 = w_quartiles[1],
      w_q25 = w_quartiles[2],
      w_q50 = w_quartiles[3],
      w_q75 = w_quartiles[4],
      w_q95 = w_quartiles[5]
    )
  })
  
  
  w_quartiles <- weighted_quantile(mcmle_filtered$LL, rep(1,nrow(mcmle_filtered)), c(0.05, 0.25, 0.5, 0.75, 0.95))
  
  results%<>%rbind(data.frame(
    variable = "LL",
    w_mean = mean(mcmle_filtered$LL),
    w_q05 = w_quartiles[1],
    w_q25 = w_quartiles[2],
    w_q50 = w_quartiles[3],
    w_q75 = w_quartiles[4],
    w_q95 = w_quartiles[5]
  ))
  
  # **3. Combine all results into a single dataframe**
  bind_rows(bind_rows(results), bind_rows(hsev_isev_results))
}
