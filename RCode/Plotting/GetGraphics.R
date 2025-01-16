#..................  Graphics script ....................

GET_WB_INCOME <- function(syear=1990, fyear=NULL){ 
  # Set the upper limit for the year
  if(is.null(fyear)) fyear <- AsYear(Sys.Date())
  
  # WB INCOME GROUP DATASET
  URL <- "https://datacatalogapi.worldbank.org/ddhxext/ResourceDownload?resource_unique_id=DR0090754"
  # Where to store it to
  outloc <- "./Data/RawData/WB/INCOME_GROUPS.xlsx"
  # Download it and save it out
  download.file(URL, outloc, mode = "wb")
  
  WB <- read_excel("./Data/RawData/WB/INCOME_GROUPS.xlsx", sheet = "Country Analytical History")[,-2]
  colnames(WB) <- c("ISO3.CODE", WB[5,-1])
  
  WB_INCOME <- WB %>%
    filter(
      !is.na(ISO3.CODE)
    ) %>%
    pivot_longer(
      cols = -ISO3.CODE,
      names_to = "Year",
      values_to = "INCOME_GROUP"
    ) %>%
    mutate(
      INCOME_GROUP = case_when(
        INCOME_GROUP == "L" ~ "Low",
        INCOME_GROUP == "LM" ~ "Lower Middle",
        INCOME_GROUP == "UM" ~ "Upper Middle",
        INCOME_GROUP == "H" ~ "High",
        TRUE ~ INCOME_GROUP
      )
    ) %>%
    filter(
      Year >= syear,
      Year <= fyear
    )
  
  return(WB_INCOME)
}

Get_AgriGDP <- function(syear=1990, fyear=NULL){
  # Set the upper limit for the year
  if(is.null(fyear)) fyear <- AsYear(Sys.Date())
  
  # WB INCOME GROUP DATASET
  URL <- "https://bulks-faostat.fao.org/production/Value_of_Production_E_All_Data.zip"
  # Where to store it to
  outloc <- "./Data/RawData/FAOSTAT_AgriGDP.zip"
  # Download it and save it out
  download.file(URL, outloc, mode = "wb")
  # Unzip it
  unzip(outloc,exdir = str_split(outloc,".zip",simplify = T)[1,1])
  
  #US CPI 
  CPI_CONV <- GetCPI_USA(syear,fyear)
  
  #COUNTRIES AND ITEMS
  FAOSTAT <- GetFAOSTAT_Prod(syear=syear,fyear=fyear)$yield_data
  
  # Load the data
  AGRI_GDP <- read.csv("./Data/RawData/FAOSTAT_AgriGDP/Value_of_Production_E_All_Data.csv")%>%
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
    Year = as.numeric(str_remove(Year, "^Y")),
    M49 = as.numeric(gsub("'", "", Area.Code..M49.)),
    ISO3.CODE =  countrycode(
      M49, 
      origin = "un",
      destination = "iso3c"
    )
  )%>%
    filter(
      Element == "Gross Production Value (current thousand US$)",
      ISO3.CODE %in% FAOSTAT$ISO3.CODE,
      Item %in% FAOSTAT$Item #only GDP from Items considered in the analysis
    )%>%
    left_join(
      CPI_CONV,
      by = "Year"
    )%>%
    transmute(
      ISO3.CODE,
      Year,
      Item,
      USD_GDP = (Value/CPI_2017)*1000
    )
  
  return(AGRI_GDP)
}

