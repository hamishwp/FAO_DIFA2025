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
