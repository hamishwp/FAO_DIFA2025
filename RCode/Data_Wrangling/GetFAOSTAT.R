GetFAOSTAT<-function(syear=1990,fyear=NULL){
  # Set the upper limit for the year
  if(is.null(fyear)) fyear<-AsYear(Sys.Date())
  # FAOSTAT data extract link
  URL<-"https://bulks-faostat.fao.org/production/Production_Crops_Livestock_E_All_Data.zip"
  # Where to store it to
  outloc<-"./Data/RawData/FAOSTAT.zip"
  # Download it and save it out
  download.file(URL,outloc)
  # Unzip it
  unzip(outloc,exdir = str_split(outloc,".zip",simplify = T)[1,1])
  # Load the data
  read.csv("./Data/RawData/FAOSTAT/Production_Crops_Livestock_E_All_Data.csv")
}