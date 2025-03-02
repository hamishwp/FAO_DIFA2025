# Welcome to the FAO DIFA repository!
# Developed by:
# - Hamish Patten
# - Ignacio Acosta
# - Priti Rajagopalan
# - Nina Deliu

# Start and end year of analysis
syear=1991
fyear=2023
# Do we want to use the Desinventar data to infer disaster severity? If not, use EM-DAT multivariate model
Desinventar<-T
# MCMC hyperparameters
hyppars<-list(chains=8,iter=3000,burnin=1000,adapt=0.95,maxtree=30)
# Load the packages & default functions
source("./RCode/Setup/GetPackages.R")
source("./RCode/Setup/Functions.R")
# Which STAN model to use?
stan_model_code <- "./RCode/Models/DIFA2025_redredDisSev_empAR_V2.stan" 
iprox_dat <- ifelse(grepl("redDisSev",stan_model_code),F,T)
GPR <- ifelse(!(grepl("noGPR",stan_model_code) | grepl("empAR",stan_model_code)),T,F)
empAR <- ifelse(grepl("empAR",stan_model_code),T,F)
# Save all files with this time-dependent extension
save_str<-paste0("_",str_replace_all(str_replace_all(Sys.time()," ","_"),":",""))
# Methodology to parameterise the model (can be 'MCMC', 'Optim' or 'VI'):
methody <- "MCMC"

# Run the stan model
TrainModel<-function(fdf,model){
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

# Main function
execDIFA<-function(method="MCMC",presave=T){
  # Extract, transform then merge data (functions found in 'RCode/Data_Wrangling/')
  if(presave & file.exists("./Data/Results/difa2025.RData")) {difa<-readRDS("./Data/Results/difa2025.RData") 
  } else difa<-getData(syear=syear,fyear=fyear)
  # Train the disaster severity model and predict on EM-DAT+UCDP data
  if(presave & file.exists("./Data/Results/sevvies.RData")) {sevvies<-readRDS("./Data/Results/sevvies.RData") 
  } else sevvies<-GetDisSev(difa$dissie$dessie,difa$dissie$emdat)%>%
    ConvHe2Tonnes(difa$faostat)
  # Prepare the data to be input into the stan model
  fdf<-Prepare4Model(difa$faostat,sevvies,fyear=fyear,syear=syear)
  # Train the model
  if(method=="MCMC") {
    mGPR<-TrainModel(fdf=fdf,model=stan_model_code)
  } else if(method=="VI"){
    mGPR<-TrainModel_VI(fdf=fdf,model=stan_model_code)
  } else {
    mGPR<-TrainModel_Optim(fdf=fdf,model=stan_model_code)
  }
  
  return(list(difa=difa,
              fdf=fdf,
              mGPR=mGPR))
}

mcmc_results<-execDIFA(method=methody)
saveRDS(mcmc_results,paste0("./Data/Results/fullresults_",str_split(str_split(stan_model_code,"/")[[1]][4],".stan")[[1]][1],save_str,".RData"))

###### TODAY ######
# Initial values... are you happy with them yet?
# Run noGPR models
# Run stan code on magpie
# Model the price data using : y ~ (1 + time | ISO3) + (1 + time | haz_grp)
# Add datasets into stan file to calculate the losses in production in USD via the generated_quantities?
