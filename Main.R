# Welcome to the FAO DIFA repository!
# Developed by:
# - Priti Rajagopalan
# - Nina Deliu
# - Ignacio Acosta
# - Hamish Patten

# Start and end year of analysis
syear=1990
fyear=AsYear(Sys.Date())
# Do we want to use the Desinventar data to infer disaster severity? If not, use EM-DAT multivariate model
Desinventar<-T
# Load the packages & default functions
source("./RCode/Setup/GetPackages.R")
source("./RCode/Setup/Functions.R")
# Which STAN model to use?
stan_model_code <- "./RCode/Models/DIFA2025.stan" 
  
# Main function
execDIFA<-function(){
  # Extract, transform then merge data (functions found in 'RCode/Data_Wrangling/')
  difa<-getData(syear=syear,fyear=fyear)
  # Train the disaster severity model and predict on EM-DAT+UCDP data
  sevvies<-GetDisSev(difa$dissie$dessie,difa$dissie$emdat)%>%
    convHe2Tonnes(difa$faostat)
  # Prepare the data to be input into the stan model
  fdf<-Prepare4Model(difa,sevvies)
  # Train the model
  mGPR<-TrainModel(df=fdf,model=stan_model_code)
  
  return(list(difa=difa,
              fdf=fdf,
              mGPR=mGPR))
}
