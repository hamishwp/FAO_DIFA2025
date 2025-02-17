
GetWorldBank<-function(syear=1990,fyear=NULL){
  # Set final year to current year
  if(is.null(fyear)) fyear<-AsYear(Sys.Date())
  # Which indicators do we want?
  indies<-c("SP.POP.TOTL", # total population
            "NY.GDP.MKTP.KD", # GDP 2015 $-constant
            "AG.SRF.TOTL.K2") # country surface area
  # Extract each of them, one-by-one
  wb<-do.call(rbind,lapply(indies,function(i){
    value<-wbstats::wb_data(indicator = i, 
                            start_date = as.character(syear), 
                            end_date = as.character(fyear))
    # Get rid of unused features
    value%<>%transmute(iso3=iso3c,date=date,value=get(i), indicator=i)
    # Housecleaning!
    attr(value$value,"label")<-NULL
    
    return(value)
  }))%>%pivot_wider(names_from = indicator, values_from=c(value))
  # Neaten up column names
  colnames(wb)<-c("ISO3","year","population","GDP","surfarea")
  
  return(wb)
}