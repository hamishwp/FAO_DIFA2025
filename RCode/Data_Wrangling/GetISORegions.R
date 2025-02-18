
GetISOregion<-function(){
  # This file comes originally from UNICEF regional database that can be found online
  isoreg<-read_xlsx("./Data/Taxonomies/IsoContinentRegion.xlsx")
  # Rename columns to be more appropriate
  colnames(isoreg)<-c("ISO3","country","unregion","unsubregion","sdgregion","devregion","unicefregion","unicefsubregion","whoregion","incomegrp","incomecomb","wbregion")
  # Grouping 'Americas': they stupidly didn't realise Mexico is North America
  isoreg$unregion[isoreg$unregion=="Latin America and the Caribbean"]<-"Americas"
  isoreg$unregion[isoreg$unregion=="North America"]<-"Americas"
  
  return(isoreg)
}