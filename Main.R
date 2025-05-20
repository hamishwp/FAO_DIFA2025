# Welcome to the FAO DIFA repository!
# Developed by:
# - Hamish Patten
# - Ignacio Acosta
# - Priti Rajagopalan
# - Nina Deliu

# Start and end year of analysis
syear=1991
fyear=2023
# Maximum number of disasters per country to be included
mxdis <- 30
# Do we want to use the Desinventar data to infer disaster severity? If not, use EM-DAT multivariate model
Desinventar<-T
# MCMC hyperparameters
hyppars<-list(chains=8,iter=5000,burnin=1500,adapt=0.95,maxtree=30)
# Methodology to parameterise the model (can be 'MCMC', 'Optim' or 'VI'):
methody <- "MCMLE"
# Load the packages & default functions
source("./RCode/Setup/GetPackages.R")
source("./RCode/Setup/Functions.R")
# Which STAN model to use?
stan_model_code <- "./RCode/Models/DIFA2025_log_empAR_simDS_V1.stan" 
iprox_dat <- ifelse(grepl("redDisSev",stan_model_code),F,T); GPR <- ifelse(!(grepl("noGPR",stan_model_code) | grepl("empAR",stan_model_code)),T,F); empAR <- ifelse(grepl("empAR",stan_model_code),T,F)
# Save all files with this time-dependent extension
save_str<-paste0("_",str_replace_all(str_replace_all(Sys.time()," ","_"),":",""))

# Main function
execDIFA<-function(method="MCMC",presave=T){
  if(presave & file.exists("./Data/Results/fdf.RData")) {
    fdf<-readRDS("./Data/Results/fdf.RData")
    fdf$mxdis<-mxdis
    fdf$n_dis<-pmin(mxdis,fdf$n_dis)
  } else {
    # Extract, transform then merge data (functions found in 'RCode/Data_Wrangling/')
    if(presave & file.exists("./Data/Results/difa2025.RData")) {difa<-readRDS("./Data/Results/difa2025.RData") 
    } else difa<-getData(syear=syear,fyear=fyear)
    # Train the disaster severity model and predict on EM-DAT+UCDP data
    if(presave & file.exists("./Data/Results/sevvies.RData")) {sevvies<-readRDS("./Data/Results/sevvies.RData") 
    } else sevvies<-GetDisSev(difa$dissie$dessie,difa$dissie$emdat)%>%
        ConvHe2Tonnes(difa$faostat)
    # Prepare the data to be input into the stan model
    fdf<-Prepare4Model(difa$faostat,sevvies,fyear=fyear,syear=syear,mxdis = mxdis)
    fdf$mxdis<-mxdis
    rm(difa)
  }
  # Train the model
  mGPR<-TrainModel(fdf=fdf%>%ModMxDis(),
                   model=stan_model_code,method=method)
  
  return(list(fdf=fdf,
              mGPR=mGPR))
}

mcmc_results<-execDIFA(method=methody)
saveRDS(mcmc_results,paste0("./Data/Results/fullresults_",str_split(str_split(stan_model_code,"/")[[1]][4],".stan")[[1]][1],save_str,".RData"))


# Main function
execDIFA_Des<-function(method="MCMC",presave=T){
  # Extract, transform then merge data (functions found in 'RCode/Data_Wrangling/')
  if(presave & file.exists("./Data/Results/difa2025.RData")) {difa<-readRDS("./Data/Results/difa2025.RData") 
  } else difa<-getData(syear=syear,fyear=fyear)
  # Train the disaster severity model and predict on EM-DAT+UCDP data
  sevvies<-GetDisSev_Des(difa$dissie$dessie)%>%
      ConvHe2Tonnes(difa$faostat)%>%
    filter(!is.na(mu) & !is.infinite(mu) & !is.na(haz_grp))%>%
    mutate(sd=1e-6)%>%
    arrange(desc(mu))
  # Prepare the data to be input into the stan model
  fdf<-Prepare4Model(difa$faostat,sevvies,fyear=fyear,syear=syear,mxdis = mxdis)
  fdf$mxdis<-mxdis
  rm(difa)
  # Train the model
  mGPR<-TrainModel(fdf=fdf%>%ModMxDis(),
                   model=stan_model_code,method=method)
  
  return(list(fdf=fdf,
              mGPR=mGPR))
}

des_results<-execDIFA_Des(method=methody)
saveRDS(des_results,paste0("./Data/Results/Desinventar_results_",str_split(str_split(stan_model_code,"/")[[1]][4],".stan")[[1]][1],save_str,".RData"))





