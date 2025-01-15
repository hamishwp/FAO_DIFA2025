GetFAOSTAT_PRICES<-function(syear=1990,fyear=NULL){
  # Set the upper limit for the year
  if(is.null(fyear)) fyear<-AsYear(Sys.Date())
  # FAOSTAT data extract link
  URL<-"https://bulks-faostat.fao.org/production/Prices_E_All_Data.zip"
  # Where to store it to
  outloc<-"./Data/RawData/FAOSTAT_PRICES.zip"
  # Download it and save it out
  download.file(URL,outloc)
  # Unzip it
  unzip(outloc,exdir = str_split(outloc,".zip",simplify = T)[1,1])
  
  PRICES <- read.csv("./Data/RawData/FAOSTAT_PRICES/Prices_E_All_Data.csv")%>%
    select(
      -matches(
        "F$|N$"
      )
    )%>%
    pivot_longer(
      cols = starts_with("Y"), 
      names_to = "Year",   
      values_to = "Value"  
    )%>%
    mutate(
      Year = str_remove(Year, "^Y")
    )
}