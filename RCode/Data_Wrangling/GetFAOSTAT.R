###################### ITEM CATEGORIES

## adding the grouping of 2021 report
# Cereals = c(44, 89, 101, 108, 94, 103, 56, 79, 75, 92, 27, 71, 83, 97, 15)
Cereals = c(44, 89, 101, 94, 103, 56, 79, 75, 92, 27, 71, 83, 97, 15)

Coffee_tea_cocoa_spice = c(711, 689, 693, 698, 661, 656, 720, 671, 702, 687, 723, 667, 692)

Fruits_nuts = c(221, 515, 526, 226, 572, 486, 558, 552, 216, 217, 591,
                531, 530, 220, 554, 550, 577, 569, 512, 542, 603, 549,
                507, 560, 225, 592, 224, 497, 571, 243, 490, 600, 534,
                521, 587, 574, 223, 489, 536, 523, 547, 544, 495, 222, 1729,
                619,541,234)

Hides_skins = c(957, 919, 1025, 1044, 995)

Legumes = c(203, 176, 181, 191, 195, 201, 210, 187, 197, 211, 205)

Oilseeds = c(265, 249, 329, 242, 336, 277, 310, 311, 263, 333, 299,
             292, 254, 257, 339, 260, 256, 296, 270, 280, 328, 289,
             236, 267, 305, 275)

# Meat = c(949, 1129, 869, 1019, 1037, 979, 1122, 1084, 972, 1137, 944,
#          1094, 1070, 1077, 1032, 1120, 1124, 1161, 1055, 1144, 1154,
#          1012, 1087, 1166, 1108, 1089, 947, 1127, 867, 1058, 1069, 1163,
#          1017, 1073, 1097, 1111, 1158, 1151, 1035, 1151, 977, 1080, 948,
#          1128, 868, 1018, 1098, 1036, 978, 1185, 1176,
#          1806,1808,1807,1141)
Meat = c(1127, 1017, 977, 1806, 1058, 1035, 1080, 1141, 
         1108, 1069, 1073, 1111, 1158, 1151, 1089)

Milk_honey_eggs = c(1183, 1062, 1067, 1091, 1092, 1182, 951, 1130, 882, 1020, 982, 987)

Roots_tuber =c(125, 116, 149, 122, 136, 137, 135)

Sugar_crops = c(157, 156, 161)

tobacco_fibres_rubber =c(461, 459, 839, 677, 748, 754, 836, 826,
                         782,773,789,821,777,780,788,800,809)

vegetables = c(366, 367, 414, 358, 426, 378, 393, 401, 397,
               399, 406, 407, 372, 446, 568, 449, 430, 403,
               402, 417, 394, 373, 423, 388, 463, 420, 567)

GetFAOSTAT_All<-function(syear=1990,fyear=NULL){
  # Set the upper limit for the year
  if(is.null(fyear)) fyear<-AsYear(Sys.Date())
  # Production & prices
  FAOSTAT <- GetFAOSTAT_Prod(syear=syear,fyear=fyear)
  
  return(list(yield = FAOSTAT$yield_data,
              MF = FAOSTAT$MF,
              Area = FAOSTAT$Area,
              Prod = FAOSTAT$Prod,
              price=GetFAOSTAT_Price(syear=syear,fyear=fyear)))
}

GetFAOSTAT_Prod<-function(syear=1990,fyear=NULL){
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
      )%>%
    filter(
      Year >= syear,
      Year <= fyear
    )
  
  CleanFAOSTAT(DATA)

}

GetCPI_USA <- function(syear=1990,fyear=NULL){
 
   ######## CPI USD
  WB_cpi.lst <- getWDItoSYB(
    indicator = "FP.CPI.TOTL", 
    name = "FP.CPI.TOTL"
  )["entity"]%>%
    as.data.frame()%>%
    filter(
      entity.ISO2_WB_CODE == "US"
    )%>%
    select(
      -entity.ISO2_WB_CODE,
      -entity.Country
    )%>%
    rename(
      Year = entity.Year,
      CPI_2010 = entity.FP.CPI.TOTL
    )
  
  CPI_2017_CF <- WB_cpi.lst%>%
    filter(
      Year == 2017
    )%>%
    pull(
      CPI_2010
    )
  
  CPI_CONV <- WB_cpi.lst%>%
    mutate(
      CPI_2017 = CPI_2010/CPI_2017_CF
    )%>%
    select(
      -CPI_2010
    )
  
  return(CPI_CONV)
}

GetFAOSTAT_Price<-function(syear=1990,fyear=NULL){
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
  
  CPI_CONV <- GetCPI_USA(syear,fyear)
  
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
      Year = as.numeric(str_remove(Year, "^Y"))
    )%>%
    filter(
      Element == "Producer Price (USD/tonne)"
    )%>%
    left_join(
      CPI_CONV,
      by = "Year"
    )%>%
    mutate(
      Price17eq = Value/CPI_2017
    )%>%
    mutate(
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
        Area == "Sudan (former)" ~ "SDN",
        TRUE ~ codenew_II
      ),
      item_grouping_f = case_when(
        Item.Code %in% Cereals | Item.Code %in% Sugar_crops ~ "Cereals & sugar crops",
        Item.Code %in% Fruits_nuts ~ "Fruits & nuts",
        Item.Code %in% Legumes ~ "Legumes",
        Item.Code %in% Meat ~ "Meat",
        Item.Code %in% Roots_tuber ~ "Roots & tubers",
        Item.Code %in% vegetables ~ "Vegetables")
    )%>%
    select(
      ISO3.CODE,
      Year,
      Item,
      Price17eq,
      item_grouping_f
    )
  
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
  
  MF <- FAO_F1%>%
    filter(
      !( #Remove all Elements which are not multiplying factors
        Element == "Yield" |
          Element == "Yield/Carcass Weight"|
          Element == "Production" |
          Element == "Stocks"
      )
    )%>%
    mutate(
      multiply_fact = fifelse(
        Unit == "1000 Head" | Unit == "1000 No",
        Value*1000,
        Value
      ),
      multiply_fact = fifelse(
        Item=="Natural honey"|Item=="Beeswax",
        1,
        multiply_fact
      )
    )%>%
    select(
      multiply_fact,
      ISO3.CODE,
      Item,
      Year
    )%>%
    arrange(
      ISO3.CODE,
      Year,
      Item
    )

  AREA <- FAO_F1%>%
    filter(
      Element == "Area harvested"
    )
  
  
  Production <- FAO_F1%>%
    filter(
      Element == "Production"
    )%>%
    transmute(
      ISO3.CODE,
      Item,
      Year,
      Unit,
      Production = fifelse(
        Unit == "1000 No",
        Value*1000,
        Value
      ),
      Item.Code,
      item_grouping_12 = case_when(
        Item.Code %in% Cereals ~ "Cereals",
        Item.Code %in% Coffee_tea_cocoa_spice ~ "Coffee, tea, cocoa & spice crops",
        Item.Code %in% Fruits_nuts ~ "Fruits & nuts",
        Item.Code %in% Hides_skins ~ "Hides & skins",
        Item.Code %in% Legumes ~ "Legumes",
        Item.Code %in% Oilseeds ~ "Oilseeds",
        Item.Code %in% Meat ~ "Meat & meat products",
        Item.Code %in% Milk_honey_eggs ~ "Milk, Honey & eggs",
        Item.Code %in% Roots_tuber ~ "Roots & tubers",
        Item.Code %in% Sugar_crops ~ "Sugar crops",
        Item.Code %in% tobacco_fibres_rubber ~ "Tobacco, Rubber & Fibre crops",
        Item.Code %in% vegetables ~ "Vegetables"),
      item_grouping_2 = case_when(
        Item.Code %in% Cereals ~ "Plant_based",
        Item.Code %in% Coffee_tea_cocoa_spice ~ "Plant_based",
        Item.Code %in% Fruits_nuts ~ "Plant_based",
        Item.Code %in% Hides_skins ~ "Livestock_products",
        Item.Code %in% Legumes ~ "Plant_based",
        Item.Code %in% Oilseeds ~ "Plant_based",
        Item.Code %in% Meat ~ "Livestock_products",
        Item.Code %in% Milk_honey_eggs ~ "Livestock_products",
        Item.Code %in% Roots_tuber ~ "Plant_based",
        Item.Code %in% Sugar_crops ~ "Plant_based",
        Item.Code %in% tobacco_fibres_rubber ~ "Plant_based",
        Item.Code %in% vegetables ~ "Plant_based"),
      item_grouping_f = case_when(
        Item.Code %in% Cereals | Item.Code %in% Sugar_crops ~ "Cereals & sugar crops",
        Item.Code %in% Fruits_nuts ~ "Fruits & nuts",
        Item.Code %in% Legumes ~ "Legumes",
        Item.Code %in% Meat ~ "Meat",
        Item.Code %in% Roots_tuber ~ "Roots & tubers",
        Item.Code %in% vegetables ~ "Vegetables")
    )%>%
    filter(
      !is.na(Item.Code) 
    )
  
  item_groups<-Production%>%
    transmute(Item=Item,Item.Code=Item.Code,
              item_grouping_f = case_when(
                Item.Code %in% Cereals | Item.Code %in% Sugar_crops ~ "Cereals & sugar crops",
                Item.Code %in% Fruits_nuts ~ "Fruits & nuts",
                Item.Code %in% Legumes ~ "Legumes",
                Item.Code %in% Meat ~ "Meat",
                Item.Code %in% Roots_tuber ~ "Roots & tubers",
                Item.Code %in% vegetables ~ "Vegetables"))%>%distinct()%>%
    filter(!is.na(item_grouping_f))
  
  return(list(yield_data = YIELD, MF = MF,Prod = Production,Area = AREA,item_groups=item_groups))
}

GetModelProduction<-function(syear=1990,fyear=NULL){
  # Set the upper limit for the year
  if(is.null(fyear)) fyear<-AsYear(Sys.Date())
  # Production & prices
  FAOSTAT <- GetFAOSTAT_Prod(syear=syear,fyear=fyear)
  
  FAOSTAT_PROD = FAOSTAT$Prod
  
  Model_Data <- FAOSTAT_PROD%>%
    filter(
      Unit == "t"
    )%>%
    group_by(
      item_grouping_2,
      Year,
      ISO3.CODE
    )%>%
    summarise(
      Production = sum(Production,na.rm=TRUE)
    )
  
  return(Model_Data)

}

GetFAOSTAT_ER<-function(syear=1990,fyear=NULL){
  # Set the upper limit for the year
  if(is.null(fyear)) fyear<-AsYear(Sys.Date())
  # FAOSTAT data extract link
  URL<-"https://bulks-faostat.fao.org/production/Exchange_rate_E_All_Data.zip"
  # Where to store it to
  outloc<-"./Data/RawData/Exchange_rate_E_All_Data.zip"
  # Download it and save it out
  download.file(URL,outloc)
  # Unzip it
  unzip(outloc,exdir = str_split(outloc,".zip",simplify = T)[1,1])
  
  ER <- read.csv("./Data/RawData/Exchange_rate_E_All_Data/Exchange_rate_E_All_Data.csv")%>%
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
      Year = as.numeric(str_remove(Year, "^Y"))
    )%>%
    filter(
      Element == "Local currency units per USD",
      Months == "Annual value",
      !is.na(Value)
    )%>%
    mutate(
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
    transmute(
      Year,
      ISO3.CODE,
      ER = Value
    )%>%
    filter(
      !is.na(ISO3.CODE),
      Year >= syear,
      Year <= fyear
    )
}

