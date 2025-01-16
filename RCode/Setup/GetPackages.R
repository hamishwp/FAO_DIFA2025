directory<-paste0(getwd(),"/")
# Prep work
dir.create("./Data/RawData/",showWarnings = F)

GetSourceFiles<-function(){
  
  #@@@@@ SOURCE FILES @@@@@#
  # Basic functions:
  source(paste0(directory,'RCode/Setup/Functions.R'))
  if(!file.exists(paste0(directory,'RCode/Setup/GetEnv.R'))) 
    file.copy(paste0(directory,'RCode/Setup/GetEnv_Example.R'),
              paste0(directory,'RCode/Setup/GetEnv.R'),overwrite = F)
  source(paste0(directory,'RCode/Setup/GetEnv.R'))
  # Disaster related:
  source(paste0(directory,'RCode/Data_Wrangling/GetDesinventar.R'))
  source(paste0(directory,'RCode/Data_Wrangling/GetEMDAT.R'))
  source(paste0(directory,'RCode/Data_Wrangling/GetGIDD.R'))
  # Food and agriculture related:
  source(paste0(directory,'RCode/Data_Wrangling/GetFAOSTAT.R'))
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
  
}

GetPackages<-function(){

  list.of.packages <- c("devtools","dplyr", "ggplot2","tidyverse","magrittr","stringr",
                        "RColorBrewer", "reshape2","countrycode", 'doParallel', 'abind',
                        'openxlsx',"plotly","openxlsx","pracma","ghql","jsonlite",
                        "rworldmap","rworldxtra","data.table","FAOSTAT","readxl")
  
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)>0) install.packages(new.packages, repos='http://cran.us.r-project.org')
  
  # if(length(list.of.packages[!("countrycodes" %in% installed.packages()[,"Package"])])){devtools::install_github("vincentarelbundock/countrycode")}
  
  LoadLibraries()
  GetSourceFiles()
  
}

GetPackages()

# # Check the structure of the repository
# filers<-c(paste0(directory,"Plots"))
# # Make sure these files exist
# tmp<-vapply(filers, function(fff) dir.create(fff, showWarnings = FALSE),numeric(1)) ; rm(tmp)

