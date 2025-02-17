
# Which disaster datasets do we want?
GetDisaster<-function(syear=1990,fyear=NULL){
  # Set final year to current year
  if(is.null(fyear)) fyear<-AsYear(Sys.Date())
  # EM-DAT
  emdat<-API_EMDAT(syear=syear,fyear=fyear)%>%distinct()%>%arrange(ev_sdate)%>%filter(!is.na(haz_Ab))
  # Desinventar
  dessie<-GetDesinventar(forcer=F)%>%filter(!is.na(haz_Ab))%>%distinct()
  
  return(list(emdat=emdat,dessie=dessie))
}

# Wrangle the data but don't yet merge it
getData<-function(syear=1990,fyear=NULL){
  # Set final year to current year
  if(is.null(fyear)) fyear<-AsYear(Sys.Date())
  # Disasters (EM-DAT and Desinventar) 
  dissie<-GetDisaster(syear=syear,fyear=fyear)
  # Get yield from FAOSTAT
  faostat<-GetFAOSTAT_All(syear=syear,fyear=fyear)
  # Get yearly population and GDP data from World Bank to use in normalisations
  wb<-GetWorldBank(syear=syear,fyear=fyear)
  
  return(list(dissie=dissie,faostat=faostat,wb=wb))
}
