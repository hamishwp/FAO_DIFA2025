TrainModel<-function(fdf,model,method="MCMC"){
  # Train the model
  if(method=="MCMC") {
    mGPR<-TrainModel_MCMC(fdf=fdf,model=stan_model_code)
  } else if(method=="VI"){
    mGPR<-TrainModel_VI(fdf=fdf,model=stan_model_code)
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