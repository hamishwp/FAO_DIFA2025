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
  DATA <- read.csv("./Data/RawData/FAOSTAT/Production_Crops_Livestock_E_All_Data.csv")%>%
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
  
  CleanFAOSTAT(DATA)
  

}

CleanFAOSTAT <- function(FAOSTAT){
  
  #Regions: NOT to be included in the analysis. 
  
  regions = c( "Western Africa",                                      
               "Western Asia",                                        
               "Western Europe",                                      
               "World",        
               "Southern Africa",                                     
               "Southern Asia",                                       
               "Southern Europe",
               "South America", 
               "South-eastern Asia",
               "Small Island Developing States",
               "Oceania", 
               "Northern Africa",                                     
               "Northern America",                                    
               "Northern Europe", 
               "Net Food Importing Developing Countries", 
               "Middle Africa",
               "Low Income Food Deficit Countries", 
               "Least Developed Countries", 
               "Land Locked Developing Countries" ,                   
               "Europe" ,                                             
               "European Union (27)",  
               "Eastern Africa",                                      
               "Eastern Asia",                                        
               "Eastern Europe",
               "Central America",                                     
               "Central Asia", 
               "Caribbean",
               "Australia and New Zealand",
               "Asia", "Americas", "Africa",
               "Melanesia",
               "Polynesia",
               "Micronesia",
               "China", #Added China here so we keep dissagregation by regions (Taiwan, HK, etc) - It is like this in the EM-DAT dataset
               "Sudan (former)"
  ) 
  
  
  # Umbrella categories: not to be considered
  
  umbrella_categories = c("Cereals,Total",
                          "Cereals, Total", # with the space after the commas
                          "Cereals, primary",
                          "Citrus Fruit,Total",
                          "Citrus Fruit, Total", # with the space after the commas
                          "Coarse Grain, Total",
                          "Crops Primary",
                          "Fibre Crops Primary",
                          "Fruit excl Melons,Total",
                          "Fruit Incl Melons",
                          "Fruit Primary",
                          "Jute & Jute-like Fibres",
                          "Oilcakes Equivalent",
                          "Oilcrops, Cake Equivalent",
                          "Oilcrops, Oil Equivalent",
                          "Fibre Crops, Fibre Equivalent",
                          "Oilcrops Primary",
                          "Pulses,Total",
                          "Pulses, Total", # with the space after the commas
                          "Roots and Tubers,Total",
                          "Roots and Tubers, Total", # with the space after the commas
                          "Treenuts,Total",
                          "Treenuts, Total", # with the space after the commas
                          "Vegetables Primary",
                          "Vegetables&Melons, Total",
                          "Milk, Total",
                          "Sugar Crops Primary",
                          "Eggs Primary")
  
  # Filtering out regions and umbrella categories - Creating/correcting ISO3.CODE.
  
  FAO_F1 <- FAOSTAT%>%
    filter(
      !(Area %in% regions),
      !(Item %in% umbrella_categories)
    )%>%
    mutate(
      Item = fifelse(
        Item.Code == 671,
        "Maté",
        Item
      ),
      Area.Code = Area.Code,
      codenew_I = case_when( #We change Area.Code so this countries corresponds to the ones they became later (see below)
        Area.Code == 62 ~ 238,
        Area.Code == 277 ~ 276,
        Area.Code == 15 ~ 255,
        TRUE ~ Area.Code
      ),
      codenew_II = countrycode(
        codenew_I, 
        origin = "fao",
        destination = "iso3c"
      ),
      ISO3.CODE = case_when(
        Area == "Belgium-Luxembourg" ~ "BLX",
        Area == "China, mainland" ~ "CHN",
        Area == "Czechoslovakia" ~ "CSK",
        Area == "Palestine" ~ "PSE",
        Area == "Serbia and Montenegro" ~ "SCG",
        Area == "USSR" ~ "SUN",
        Area== "Yugoslav SFR" ~ "YUG",
        Area == "South Sudan" ~ "SSN", #Former code states that countrycodes was not differentiating between sudan and south sudan, we do it manually
        TRUE ~ codenew_II
      )
    )%>%
    select(
      -codenew_I,
      -codenew_II
    )

  ##################### YIELD DATAFRAME
  
  egg_unit = c("Hen eggs in shell, fresh","Eggs from other birds in shell, fresh, n.e.c.") #Eggs categories for which 100mg/An unit will be used
  
  YIELD <- FAO_F1%>%
    filter(
      !(#We delete Yield values for Beeswax and Natural honey, we use production as yield (yield is not complete - production is)
        Element == "Yield" & 
          (
            Item == "Beeswax" |
              Item == "Natural honey"
          )
      ),
      !(Item %in% egg_unit & Unit !="100mg/An")
    )%>%
    mutate(
      Element = fifelse(
        (
          Item == "Beeswax"|
            Item== "Natural honey"
        )& 
          Element=="Production",
        "Yield",
        Element
      ),
      Element.Code = fifelse(
        (
          Item == "Beeswax"|
            Item== "Natural honey"
        )& 
          Element=="Yield",
        5422,
        Element.Code
      )
    )%>% 
    filter(                  
      Element =="Yield"|Element == "Yield/Carcass Weight"
    )%>%
    mutate(
      Yield = case_when(
        Unit == "hg/ha" | Unit == "hg/An" | Unit == "100 g/An" | Unit == "100 g/ha" ~ Value*0.0001,
        Unit == "0.1g/An" | Unit == "100mg/An" ~ Value*0.0000001,
        Unit == "kg/ha" ~ Value * 0.001,
        TRUE ~ Value
      )
    )%>%
    select(
      ISO3.CODE,
      Item,
      Year,
      Yield
    )%>%
    arrange(
      ISO3.CODE,
      Year,
      Item
    )
  

  
}
