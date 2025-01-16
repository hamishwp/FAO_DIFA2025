# Welcome to the FAO DIFA repository!
# Developed by:
# - Priti Rajagopalan
# - Nina Deliu
# - Ignacio Acosta
# - Hamish Patten

# Load the packages & default functions
source("./RCode/Setup/GetPackages.R")
source("./RCode/Setup/Functions.R")
  
# Main function
execDIFA<-function(){
  # Extract, transform then merge data (functions found in 'RCode/Data_Wrangling/')
  difaDF<-getData()%>%mergeData()
  # Train the model
  mGPR<-TrainModel(df=difaDF,model="GPR")
  # Generate counterfactuals (depending on trained model)
  cntfcts<-counterfacts(df=difaDF,model=mGPR)
  # Add economic losses
  cntfcts%<>%addEconomics()
  # Null hypothesis testing
  nullies<-nullhyp(cntfcts$prod, difaDF$prod)
  
  return(list(difaDF=difaDF,
              mGPR=mGPR,
              cntfcts=cntfcts,
              nullies=nullies))
}
