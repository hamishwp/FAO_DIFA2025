# This file contains the functions required to extract EM-DAT data via their API
# The main function called is 'API_EMDAT' which then relies on the others

# Convert from the EM-DAT hazard names to a more general form
EMDATHazards_API<-function(EMDAT){
  EMDAT$subgroup%<>%str_to_lower()
  EMDAT$type%<>%str_to_lower()
  EMDAT$subtype%<>%str_to_lower()
  # Read in the EMDAT-HIPS taxonomy conversion dataframe
  colConv<-openxlsx::read.xlsx("./Data/Taxonomies/EMDAT_HIP_API.xlsx")
  colConv$subgroup%<>%str_to_lower()
  colConv$type%<>%str_to_lower()
  colConv$subtype%<>%str_to_lower()
  # Reduce the translated vector and merge
  EMDAT%<>%left_join(colConv,by = c("subgroup","type","subtype"),
                     relationship="many-to-one")%>%
    mutate(
      haz_Ab = ifelse(
        type == "volcanic activity",
        "VO",
        haz_Ab
      )
    )
  
  EMDAT%>%dplyr::select(-c(group,subgroup,type,subtype,associated_types,subregion))
} 

# Clean up the EM-DAT data, such as converting from day, month, year to date
CleanEMDAT_API<-function(EMDAT){
  # Some of the column names are messed up due to presence of non-letters
  EMDAT%<>%rename("ISO3" = "iso",
                  "AID.Contribution"="aid_contribution",
                  "Reconstruction.Costs"="reconstr_dam",
                  "Reconstruction.Costs.Adjusted"="reconstr_dam_adj",
                  "Insured.Damages"="insur_dam",
                  "Insured.Damages.Adjusted"="insur_dam_adj",
                  "Total.Damages"="total_dam",
                  "Total.Damages.Adjusted"="total_dam_adj",
                  "Total.Deaths"="total_deaths",
                  "No.Injured"="no_injured",
                  "No.Affected"="no_affected",
                  "No.Homeless"="no_homeless",
                  "Total.Affected"="total_affected",
                  "imp_credate"="entry_date",
                  "imp_moddate"="last_update")
  # Also, make sure to convert to the full value in US dollars
  EMDAT[,c("AID.Contribution","Reconstruction.Costs","Reconstruction.Costs.Adjusted",
           "Insured.Damages","Insured.Damages.Adjusted","Total.Damages",
           "Total.Damages.Adjusted")]<-1000*EMDAT[,c("AID.Contribution","Reconstruction.Costs","Reconstruction.Costs.Adjusted",
                                                                "Insured.Damages","Insured.Damages.Adjusted","Total.Damages",
                                                                "Total.Damages.Adjusted")]
  # For dates with no start day, make it the middle of the month
  EMDAT$start_day[is.na(EMDAT$start_day)]<-15
  # Make sure the start date is 2 characters
  EMDAT$start_day[nchar(EMDAT$start_day)==1 & !is.na(EMDAT$start_day)]<-
    paste0("0",EMDAT$start_day[nchar(EMDAT$start_day)==1 & !is.na(EMDAT$start_day)])
  # Make sure the start month is 2 characters
  EMDAT$start_month[nchar(EMDAT$start_month)==1 & !is.na(EMDAT$start_month)]<-
    paste0("0",EMDAT$start_month[nchar(EMDAT$start_month)==1 & !is.na(EMDAT$start_month)])
  # Make sure the end date is 2 characters
  EMDAT$end_day[nchar(EMDAT$end_day)==1 & !is.na(EMDAT$end_day)]<-
    paste0("0",EMDAT$end_day[nchar(EMDAT$end_day)==1 & !is.na(EMDAT$end_day)])
  # Make sure the end month is 2 characters
  EMDAT$end_month[nchar(EMDAT$end_month)==1 & !is.na(EMDAT$end_month)]<-
    paste0("0",EMDAT$end_month[nchar(EMDAT$end_month)==1 & !is.na(EMDAT$end_month)])
  # If the start month is NA, we take the start to be the start of the year
  EMDAT$start_day[is.na(EMDAT$start_month)]<-"01" 
  EMDAT$start_month[is.na(EMDAT$start_month)]<-"01"
  # If the end month is NA, we take the end to be the end of the year
  EMDAT$end_day[is.na(EMDAT$end_month)]<-"31" 
  EMDAT$end_month[is.na(EMDAT$end_month)]<-"12"
  # If the end date is NA, assume end of the month
  EMDAT$end_day[is.na(EMDAT$end_day) & !is.na(EMDAT$end_month)]<-
    sapply((1:nrow(EMDAT))[is.na(EMDAT$end_day) & !is.na(EMDAT$end_month)], function(i){
      # Go to the next month and then subtract one day to make it the end of the original month
      monthy<-as.character(as.numeric(EMDAT$end_month[i])+1)
      # Checks for the character
      if(nchar(monthy)==1) monthy<-paste0("0",monthy)
      if(monthy=="13") return("31")
      # Create the date variable, subtract one day from it then extract the date
      format(as.Date(paste0(c(EMDAT$end_year[i],monthy,
                              "01"),collapse = "-"))-1,"%d")
    },simplify = T)
  # Start date in one
  EMDAT$sdate<-EMDAT$imp_unitdate<-sapply(1:nrow(EMDAT),function(i) paste0(c(EMDAT$start_year[i],
                                                                                EMDAT$start_month[i],
                                                                                EMDAT$start_day[i]),collapse = "-"),simplify = T)
  # End date in one
  EMDAT$fdate<-sapply(1:nrow(EMDAT),function(i) paste0(c(EMDAT$end_year[i],
                                                            EMDAT$end_month[i],
                                                            EMDAT$end_day[i]),collapse = "-"),simplify = T)
  
  EMDAT$duration<-as.numeric(as.Date(EMDAT$fdate)-as.Date(EMDAT$sdate))
  # Remove everything we dont need
  EMDAT%<>%dplyr::select(-c(start_day,start_month,start_year,
                            end_day,end_month,end_year,
                            country,region))
  
  # Only saving disasters of interest
  # disaster_type_list = c( 
  #   "Drought",
  #   "Earthquake",
  #   "Extreme temperature",
  #   "Flood",
  #   "Insect infestation",
  #   "Landslide",
  #   "Storm", 
  #   "Wildfire",
  #   "Volcanic activity", 
  #   "Mass movement (dry)"
  # )
  # 
  # EMDAT%<>%filter( 
  #   type %in% disaster_type_list
  # )
  
  # Link to the hazard taxonomy from HIPS
  EMDAT%<>%EMDATHazards_API()
  
  #REMOVE IRRELEVANT VARIABLES
  
  EMDAT%<>%dplyr::select( 
    # -disno,
    -external_ids,
    -name,
    -origin,
    -ofda_response,
    -appeal,
    -declaration,
    -AID.Contribution,
    -magnitude,
    -magnitude_scale,
    -latitude,
    -longitude,
    -river_basin,
    -Reconstruction.Costs,
    -Reconstruction.Costs.Adjusted,
    -Insured.Damages,
    -Insured.Damages.Adjusted,
    -cpi,
    -admin_units,
    -imp_credate,
    -imp_moddate,
    -imp_unitdate,
    -haz_type,
    -haz_cluster,
    -haz_spec,
    -hazlink,
    -haz_potlink,
    -classif_key,
    -location,
    -No.Injured,
    -No.Homeless,
    -No.Affected,
    # -Total.Affected,
    -Total.Damages,
    # -Total.Deaths
  )
  
  colnames(EMDAT)<-c("disno","ISO3","deaths","affected","cost","sdate","fdate","duration","haz_Ab")
  
  return(EMDAT)
  
}

# Directly extract the data from the EM-DAT API
API_EMDAT<-function(syear=1991,fyear=NULL){
  # Set the upper limit for the year
  if(is.null(fyear)) fyear<-AsYear(Sys.Date())
  
  query_str = 
    'query monty {
      api_version
      public_emdat(
        cursor: {limit: -1}
        filters: {
          from: @@@@,
          to: ####,
          classif: ["nat-*"],
          include_hist: true
        }
      ) {
        total_available
        info {
          timestamp
          filters
          cursor
        }
        data {
          disno
          classif_key
          group
          subgroup
          type
          subtype
          external_ids
          name
          iso
          country
          subregion
          region
          location
          origin
          associated_types
          ofda_response
          appeal
          declaration
          aid_contribution
          magnitude
          magnitude_scale
          latitude
          longitude
          river_basin
          start_year
          start_month
          start_day
          end_year
          end_month
          end_day
          total_deaths
          no_injured
          no_affected
          no_homeless
          total_affected
          reconstr_dam
          reconstr_dam_adj
          insur_dam
          insur_dam_adj
          total_dam
          total_dam_adj
          cpi
          admin_units
          entry_date
          last_update
        }
      }
    }'
  
  query_str<-gsub("####",fyear,query_str)
  query_str<-gsub("@@@@",syear,query_str)
  # setup the connection with the GraphQL database
  client <- ghql::GraphqlClient$new(
    url = "https://api.emdat.be/v1",
    headers = list(Authorization = emdat_token)
  )
  # Setup the query in GraphQL language
  q <- ghql::Query$new()
  q$query('monty',query_str)
  # Make the query to EM-DAT
  jsonlite::fromJSON(client$exec(q$queries$monty))$data$public_emdat$data%>%
    CleanEMDAT_API()
}


# PairEMDATspatial<-function(EMDAT,haz="EQ",GAULexist=F){
#   
#   if(sum(!is.na(EMDAT$Geo.Locations))!=sum(!is.na(EMDAT$Adm.Level)) &
#      sum(!is.na(EMDAT$Geo.Locations))!=sum(!is.na(EMDAT$Admin1.Code)) & 
#      sum(!is.na(EMDAT$Geo.Locations))!=sum(!is.na(EMDAT$Admin2.Code))) stop("EMDAT has irregular admin locations")
#   # Get all the iso codes that we need admin data for
#   isoGAUL<-unique(EMDAT$ISO[!is.na(EMDAT$Geo.Locations)])
#   # If the GAUL data is already there, link it and exit
#   if(GAULexist){
#     
#   }
#   # Load all required shapefiles
#   checkers<-GetGAUL(isoGAUL)
#   # Let's patch over the ones that didn't properly work
#   GaulInc<-isoGAUL[!isoGAUL%in%list.files("./CleanedData/SocioPoliticalData/EMDAT/")]
#   # Make sure to save which ones didn't fully work
#   outer<-data.frame(ISO3C=isoGAUL,Status="Complete")
#   # Set those that didn't work
#   outer$Status[outer$ISO3C%in%GaulInc]<-"Some Elements Missing"
#   # Write out
#   write_csv(outer,paste0("./CleanedData/SocioPoliticalData/EMDAT/fully_complete_",haz,".csv"))
#   
#   EMDAT$imp_spat_ID<-checkers$ID
#   EMDAT$imp_spat_covcode<-"spat_polygon"
#   EMDAT$spat_orig<-"GAUL"
#   
#   # If some spatial objects aren't found, try accessing ADM level 1
#   if(sum(is.na(EMDAT$imp_spat_ID))!=0) {
#     # Which countries to go back over
#     inds<-is.na(EMDAT$imp_spat_ID)
#     # Try, try and try, try and tryyyyyyyyy, you'll succeed at last
#     checkers<-GetGAUL(isoGAUL[inds],lADM=1)
#     EMDAT$imp_spat_ID[inds]<-checkers$ID
#   }
#   # If some spatial objects aren't found, try accessing ADM level 0
#   if(sum(is.na(EMDAT$imp_spat_ID))!=0) {
#     # Which countries to go back over
#     inds<-is.na(EMDAT$imp_spat_ID)
#     # Try, try and try, try and tryyyyyyyyy, you'll succeed at last
#     checkers<-GetGAUL(isoGAUL[inds],lADM=0)
#     EMDAT$imp_spat_ID[inds]<-checkers$ID
#   }
#   
#   return(EMDAT)
#   
# }

GET_EMDAT_MODEL <- function(emdat=NULL){
  # Extract the data if it is not provided
  if(is.null(emdat)) emdat <- API_EMDAT()
  
  emdat <- emdat %>%
    mutate(
      sdate = as.Date(sdate),
      fdate = as.Date(fdate),
      sy = year(sdate),                     
      ey = year(fdate),                   
      s_frac = (yday(sdate) - 1) / 365,        
      e_frac = yday(fdate) / 365,             
      duration_years = as.numeric(fdate - sdate) / 365,  
      endt = sy + s_frac + duration_years     
    )
  
  years_all <- seq(min(emdat$sy), max(emdat$ey))
  
  isos <- unique(emdat$ISO3)
  
  max_events <- emdat%>% 
    group_by(ISO3)%>% 
    summarise(n = n())%>%
    pull(n)%>%
    max()
  
  ts <- array(
    NA, 
    dim = c(length(isos), length(years_all), max_events),
    dimnames = list(isos, as.character(years_all), NULL)
  )
  
  tf <- array(
    NA, 
    dim = c(length(isos), length(years_all), max_events),
    dimnames = list(isos, as.character(years_all), NULL)
  )
  
  hazdur <- array(
    NA, 
    dim = c(length(isos), length(years_all), max_events),
    dimnames = list(isos, as.character(years_all), NULL)
  )
  
  flag <- array(
    0, 
    dim = c(length(isos), length(years_all), max_events),
    dimnames = list(isos, as.character(years_all), NULL)
  )
  
  emdat%<>% 
    arrange(ISO3, sdate)%>% 
    group_by(ISO3)%>% 
    mutate(
      event_id = row_number()
    )
  
  for(i in 1:nrow(emdat)){
    iso <- emdat$ISO3[i]
    event <- emdat$event_id[i]
    sy <- emdat$sy[i]      
    ey <- emdat$ey[i]      
    sfrac <- emdat$s_frac[i]
    efrac <- emdat$e_frac[i]
    duration_years <- emdat$duration_years[i]
    endt <- emdat$endt[i]
    
    for(t in as.numeric(sy):as.numeric(max(years_all))){
      t_chr <- as.character(t)
      flag[iso, t_chr, event] <- 1
    }
    
    for(t in as.numeric(sy):as.numeric(max(years_all))){
      t_chr <- as.character(t)
      
      if(t == sy){ 
        if((1 - sfrac) >= duration_years){
          ts[iso, t_chr, event] <- 0
          tf[iso, t_chr, event] <- 1 - sfrac - duration_years
        } else { 
          ts[iso, t_chr, event] <- 1
          tf[iso, t_chr, event] <- 1
        }
        
        hazdur[iso, t_chr, event] <- duration_years
        
      } else if(t < endt){      
        ts[iso, t_chr, event] <- 1
        tf[iso, t_chr, event] <- 1
        
      } else if(t > endt & t < (endt + 1)){
        ts[iso, t_chr, event] <- 0
        tf[iso, t_chr, event] <- t - endt
      } else {
        ts[iso, t_chr, event] <- t - 1 - endt
        tf[iso, t_chr, event] <- t - endt
      }
    }
  }
  
  return(list(ts = ts,
              tf =  tf,
              hazdur = hazdur,
              flag = flag)
  )
}
