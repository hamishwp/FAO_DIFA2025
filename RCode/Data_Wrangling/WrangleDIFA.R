
# Which disaster datasets do we want?
GetDisaster<-function(){
  # EM-DAT is for-sure! Get rid of repeats and unknown data
  dissie<-API_EMDAT()%>%distinct()%>%
    arrange(ev_sdate)%>%filter(!is.na(haz_spec))
}

# Wrangle the data but don't yet merge it
getData<-function(syear=1990,fyear=2024){
  # Disasters (currently EM-DAT only) 
  dissie<-GetDisaster(syear=syear,fyear=fyear)
  # Get yield from FAOSTAT
  faostat<-GetFAOSTAT_All(syear=syear,fyear=fyear)
  # Get the USDA data
  # usda<-GetUSDA(syear=syear,fyear=fyear)
  
  # return(list(dissie=dissie,faostat=faostat,usda=usda))
  return(list(dissie=dissie,faostat=faostat))
}

# Merge the data
mergeData<-function(){
  # What format to use? Long or wide format? How to deal with commodities and different disasters?
  
  
  
}