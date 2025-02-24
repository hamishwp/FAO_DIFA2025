# Clean the UCDP dataset to get it into the same format as EM-DAT
CleanUCDP<-function(ucdp){
  # First reduce the dataframe
  ucdp%<>%dplyr::select(conflict_id,year,best_fatality_estimate,location)
  # Then translate from gwno (Gleditsch and Ward list of independent states) to ISO3C
  ucdp$location[ucdp$location=="Yemen (North Yemen)"]<-"Yemen"
  ucdp$ISO3<-convCountryIso3(ucdp$location)
  ucdp$ISO3[is.na(ucdp$ISO3)]<-convCountryIso3(str_split(ucdp$location[is.na(ucdp$ISO3)],", ",simplify = T)[,1])
  # Check all ISOs were converted
  if(length(ucdp$location[is.na(ucdp$ISO3)])>0) stop("Couldn't convert some of the countries in the UCDP dataset to ISO3C")
  # Select only certain columns and rename
  ucdp%<>%dplyr::select(-location)
  colnames(ucdp)<-c("disno","year","deaths","ISO3")
  # Add some of the date variables
  ucdp%>%mutate(sdate=paste0(year,"-01-01"),fdate=paste0(year,"-12-31"),
                haz_Ab="CF",haz_grp="CF",duration=1.,
                disno=as.character(disno),
                affected=NA,cost=NA)
}

GetUCDP<-function(syear=1990,fyear=NULL){
  # Default final year
  if(is.null(fyear)) fyear<-year(Sys.Date())
  # Define the URL for the zip file
  url <- "https://ucdp.uu.se/downloads/nsos/ucdp-onesided-241-rds.zip"
  # Create a temporary file to store the downloaded zip and a temporary directory for extraction
  temp_zip <- tempfile(fileext = ".zip")
  temp_dir <- tempfile()
  dir.create(temp_dir)
  # Download the zip file
  download.file(url, destfile = temp_zip, mode = "wb")
  # Save to raw data folder
  filer<-"./Data/RawData/UCDP"
  # Extract all files from the zip archive into the temporary directory
  unzip(temp_zip, exdir = filer)
  # Extract it!
  ucdp<-readRDS(list.files(filer,recursive = F,full.names = T))
  # Clean up the temporary zip file (optional)
  unlink(temp_zip)
  # Get it into the same format as EMDAT
  ucdp%>%CleanUCDP()%>%filter(year>=syear & year<=fyear)
}





