options(mc.cores = parallel::detectCores())
directory<-paste0(getwd(),"/")
# Prep work
dir.create("./Data/RawData/",showWarnings = F,recursive = T)
dir.create("./Plots",showWarnings = F)
dir.create("./Data/Results/Simulations",showWarnings = F,recursive = T)
dir.create("./Data/RawData/UCDP/",showWarnings = F,recursive = T)

if(!file.exists(paste0(directory,'RCode/Setup/GetEnv.R'))) 
  file.copy(paste0(directory,'RCode/Setup/GetEnv_Example.R'),
            paste0(directory,'RCode/Setup/GetEnv.R'),overwrite = F)
source(paste0(directory,'RCode/Setup/GetEnv.R'))

GetSourceFiles<-function(){
  
  #@@@@@ SOURCE FILES @@@@@#
  # Basic functions:
  source(paste0(directory,'RCode/Setup/Functions.R'))
  # Disaster related:
  source(paste0(directory,'RCode/Data_Wrangling/GetDesinventar.R'))
  source(paste0(directory,'RCode/Data_Wrangling/GetEMDAT.R'))
  source(paste0(directory,'RCode/Data_Wrangling/GetUCDP.R'))
  # Food and agriculture related:
  source(paste0(directory,'RCode/Data_Wrangling/GetFAOSTAT.R'))
  # Other key datasets required:
  source(paste0(directory,'RCode/Data_Wrangling/GetWorldBank.R'))
  source(paste0(directory,'RCode/Data_Wrangling/GetISORegions.R'))
  # Data processing functions
  source(paste0(directory,'RCode/Data_Wrangling/WrangleDIFA.R'))
  # Models required
  source(paste0(directory,'RCode/Models/DisasterSeverity_Model.R'))
  
  return(T)
}

LoadLibraries<-function(){
  
  options(stringsAsFactors = FALSE)
  
  library(dplyr)
  library(magrittr)
  library(tidyverse)
  library(readxl)
  library(ggplot2)
  # library(sp)
  # library(sf)
  # library(xml2)
  # library(ggmap)
  # library(geojsonR)
  library(countrycode)
  library(stringr)
  library(pracma)
  library(data.table)
  library(FAOSTAT)
  # library(parallel)
  # library(doParallel)
  # library(foreach)
  # library(abind)
  # library(gstat)
  # library(raster)
  # library(geosphere)
  # library(terra)
  # library(patchwork)
  # library(uuid)
  # library(lwgeom)
  
  return(T)
}

GetPackages<-function(packred=T){

  list.of.packages <- c("devtools","dplyr", "ggplot2","tidyverse","magrittr","stringr",
                        "RColorBrewer", "reshape2","countrycode", 'doParallel', 'abind',
                        'openxlsx',"plotly","openxlsx","pracma","ghql","jsonlite","kableExtra",
                        "rworldmap","rworldxtra","data.table","FAOSTAT","readxl",
                        "combinat","lme4","merTools","caret")
  
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)>0) install.packages(new.packages, repos='http://cran.us.r-project.org')
  # Install stan and rstan if packred (from Setup/GetEnv.R) is not True
  if(!packred) {
    if(!("rstan" %in% installed.packages()[,"Package"])) install.packages("rstan", repos = c('https://stan-dev.r-universe.dev', getOption("repos")))
  }
  
  LoadLibraries()
  GetSourceFiles()
  
  return(T)
}

GetPackages() 

# # Check the structure of the repository
# filers<-c(paste0(directory,"Plots"))
# # Make sure these files exist
# tmp<-vapply(filers, function(fff) dir.create(fff, showWarnings = FALSE),numeric(1)) ; rm(tmp)

