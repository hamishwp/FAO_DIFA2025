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
  fdf_vars <- c("n_isos", "n_t", "n_dis", "n_com", "n_haz", "flag", "htype", "iprox", 
                "hazdur", "ts", "tf", "lnmu_AR1", "lnsig_AR1", "lny")
  # Keep only the required elements in fdf
  fdf[names(fdf) %in% fdf_vars]
}

TrainModel_MCMLE <- function(fdf, samp = 5000, cpus = 30, LL="m_likelihood"){
  # Reduce memory of fdf list
  fdf%<>%redFdF()
  
  # Generate parameter sets
  params_list <- lapply(1:samp, function(i) list(
    sample_id = i,  # Unique ID per sample
    hsev = rgamma(fdf$n_haz, shape = 1, rate = 1),  # Hazard severity (vector)
    beta_dis = rnorm(1, mean = 0, sd = 1),  # Disaster-severity regression coefficient
    beta_dur = rgamma(1, shape = 1, rate = 1),  # Hazard duration coefficient
    isev = rgamma(fdf$n_com, shape = 1, rate = 1),  # Commodity severity (vector)
    sigma = rgamma(1, shape = 2, rate = 2),  # Standard deviation in AR1 model
    beta_y1 = rgamma(1, shape = 2, rate = 2),  # Gamma coefficient in AR1 model
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



















