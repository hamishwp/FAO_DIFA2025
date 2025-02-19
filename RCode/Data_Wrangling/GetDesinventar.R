# List of countries, clusters and territories of Desinventar data
GetDessieISOs<-function(){
  DesIsos<-data.frame(isos=
                        c("ago", "alb", "arg", "arm", "atg", "bfa", "bdi", "blr", "blz", "bol", 
                          "brb", "btn", "chl", "col", "com", "cpv", "cri", "dji", "dma", "dom",
                          "ecu", "egy", "esp", "eth", "etm", "gha", "gin", "gmb", "gnb", "gnq", 
                          "grd", "gtm", "guy", "hnd", "idn", "irn", "irq", "jam", "jor", "ken", 
                          "khm", "kna", "lao", "lbn", "lbr", "lca", "lka", "mal", "mar", "mdg", 
                          "mdv", "mex", "mli", "mmr", "mne", "mng", "moz", "mus", "mwi", "nam",
                          "ner", "nga", "nic", "npl", "pac", "pak", "pan", "per", "prt", "pry", 
                          "pse", "rwa", "sdn", "sen", "sle", "slv", "som", "srb", "swz", "sy11",
                          "syc", "syr", "tgo", "tls", "tto", "tun", "tur", "tza", "uga", "ury", 
                          "vct", "ven", "vnm", "xkx", "yem", "zmb", "znz", "019", "033", "005"))
  # try to automatically extract as many names as possible
  DesIsos%<>%mutate(country=convIso3Country(isos),
                    actualiso=str_to_upper(isos))
  # We know the annoying ISOs:
  issiso<-c("ETM", "MAL", "PAC", "SY11", "XKX", "ZNZ", "019", "033", "005")
  # Check which ones are NAs and replace them manually
  if(all(DesIsos$actualiso[is.na(DesIsos$country)]%in%issiso) & 
     length(DesIsos$actualiso[is.na(DesIsos$country)])==length(issiso)){
    DesIsos$actualiso[is.na(DesIsos$country)]<-c("TLS","MDV","PAC","SYR","XKX","TZA","IND","IND","IND")
    DesIsos$country[is.na(DesIsos$country)]<-c("Timor-Leste","Maldives",
                                               "Secretary of Pacific Community (23 countries)",
                                               "Syrian Arab Republic","Kosovo","Tanzania",
                                               "India","India","India")
  } else stop("GetDessieISOs: an extra unknown country name was found in the list")
    
  return(DesIsos)
}

GetDessie<-function(iso3,forcer=F){
  iso3%<>%str_to_lower()
  print(iso3)
  # Don't waste time if the file already exists
  if(file.exists(paste0("./RawData/MostlyImpactData/Desinventar/",iso3,"/DI_export_",iso3,".xml")) & !forcer) return(T)
  # Temporary location to store the zip file
  temp<-paste0("./RawData/tmp/tmp_",iso3,".zip")
  # Set the maximum timeout limit
  options(timeout = 60)#  options(timeout = max(10, getOption("timeout")))
  # Download the raster file to the location 'temp'
  download.file(paste0("https://www.desinventar.net/DesInventar/download/DI_export_",iso3,".zip"),temp)
  # Output location: one folder per country, to house everything
  outloc<-paste0("./RawData/MostlyImpactData/Desinventar/",iso3)
  # Check the end location exists
  if(!dir.exists(outloc)) dir.create(outloc,showWarnings = F)
  # Unpack the files in the zip document
  unzip(paste0(temp),exdir = outloc)
  
  return(T)
}

DesCols<-c('serial'='ext_ID',
           'muertos'= 'deaths',
           'hay_muertos'='flag_deaths',
           'heridos'= 'injured',
           'hay_heridos'='flag_injured',
           'desaparece'= 'missing',
           'hay_deasparece'='flag_missing',
           'vivdest'= 'houses_destroyed',
           'hay_vivdest'='flag_houses_destroyed',
           'vivafec'= 'houses_damaged',
           'hay_vivafec'='flag_houses_damaged',
           'damnificados'= 'directly_affected',
           'hay_damnificados'='flag_directly_affected',
           'afectados'= 'indirectly_affected',
           'hay_afectados'='flag_indirectly_affected',
           'reubicados'= 'relocated',
           'hay_reubicados'='flag_relocated',
           'evacuados'= 'evacuated',
           'hay_evacuados'='flag_evacuated',
           'valorus'= 'losses_in_dollar',
           'valorloc'= 'losses_local_currency',
           'nescuelas'= 'education_centers',
           'nhospitales'= 'hospitals',
           'nhectareas'= 'damages_in_crops_ha',
           'cabezas'= 'lost_cattle',
           'kmvias'= 'damages_in_roads_mts',
           'level0'= 'level0',
           'level1'= 'level1',
           'level2'= 'level2',
           'name0'= 'name0',
           'name1'= 'name1',
           'name2'= 'name2',
           'latitude' = 'latitude',
           'longitude' = 'longitude',
           'evento'= 'event',
           'glide'='GLIDE',
           'lugar'= 'location',
           'magnitud2'='haz_maxvalue',
           'duracion'='duration',
           'fechano'= 'year',
           'fechames'= 'month',
           'fechadia'= 'day',
           'ISO3'='ISO3')

RegCols<-c("codregion"="ADMcode",
           "nivel"="ADMlevel",
           "nombre"="regnamloc",
           "nombre_en"="regnamen",
           "x"="centLon",
           "y"="centLat",
           "xmin_"="mnlo",
           "ymin"="mnla",
           "xmax_"="mxlo",
           "ymax"="mxla")

ExtImpDev<-function(xmlly){
  # Extract all the impact estimate tabular information
  impacts<-do.call(dplyr::bind_rows,lapply(seq_along(xmlly$DESINVENTAR$fichas),function(i){
    return(as.data.frame(t(as.data.frame(unlist(xmlly$DESINVENTAR$fichas[[i]])))))
  })) %>% distinct(); rownames(impacts)<-NULL
  # Select and rename the columns
  impacts%<>%dplyr::select(any_of(names(DesCols)))
  impacts%<>%setNames(unname(DesCols[colnames(impacts)]))
  # Create one single data column, as a character
  impacts$date<-sapply(1:nrow(impacts),function(i) 
    as.character(as.Date(ISOdate(year = impacts$year[i],
                    month = impacts$month[i],
                    day = impacts$day[i]))))
  # Remove unnecessary columns to save space
  impacts%<>%dplyr::select(-c(year,month,day))
  # convert to integer:
  inties<-c("deaths", "injured", "missing", "houses_destroyed", 
            "houses_damaged", "directly_affected", 
            "indirectly_affected", "relocated", "evacuated", 
            "education_centers", "hospitals", "lost_cattle")
  # convert to numeric:
  nummies<-c("losses_in_dollar", "losses_local_currency", 
             "damages_in_crops_ha", "damages_in_roads_mts",
             "latitude","longitude","duration")
  # Convert all integer and numeric columns
  impacts %<>% mutate_at(inties, as.integer)
  impacts %<>% mutate_at(nummies, as.numeric)
  # by default, set minimal duration to be 1 day
  impacts$duration[is.na(impacts$duration)]<-1 
  # Ensure that un-entered impacts (where the flag = -1) are set to NA
  flaggies<-str_split(grep("flag_",colnames(impacts),value = T),"flag_",simplify = T)[,2]
  # I know it's bad practice to use for loops, but I can't be fucked, quite frankly
  for(fl in flaggies) impacts[is.na(impacts[,paste0("flag_",fl)]) | 
                                impacts[,paste0("flag_",fl)]!=-1,fl]<-NA
  # For the impacts that do not have a flag, set them to NA if they are equal to zero, just in case
  for(im in c('losses_in_dollar','losses_local_currency','education_centers',
    'hospitals','damages_in_crops_ha','lost_cattle','damages_in_roads_mts')) {
    if(im%in%colnames(impacts)) impacts[is.na(impacts[,im]) | impacts[,im]<=1e-5,im]<-NA
  }
  # Retain only the variables we need
  impacts%<>%dplyr::select(c("deaths","directly_affected",
                            "losses_in_dollar","damages_in_crops_ha","lost_cattle",
                            "duration","ISO3","sdate","fdate","haz_Ab"))
  # Clean up names
  colnames(impacts)<-c("deaths","affected","cost","crops","cattle","duration","ISO3","sdate","fdate","haz_Ab")
  
  return(impacts)
}

ReadDessie<-function(iso3, forcer=F){
  iso3%<>%str_to_lower()
  # Temporary save out location
  savout<-paste0("./RawData/MostlyImpactData/Desinventar/",iso3,"/DI_export_",iso3,"_xml.RData")
  # Extract the aggregated data
  if(file.exists(savout) & !forcer) {
    # Keep only the important columns
    impacts<-readRDS(savout)
  } else {
    xmlly<-xml2::as_list(xml2::read_xml(paste0("./RawData/MostlyImpactData/Desinventar/",iso3,"/DI_export_",iso3,".xml")))
    saveRDS(xmlly,savout)
    # Keep only the important columns
    impacts<-ExtImpDev(xmlly)
    # Store it for later
    saveRDS(impacts,savout)
  }
  # Create a folder for the results
  dir.create(paste0("./CleanedData/MostlyImpactData/Desinventar/",iso3),showWarnings = F,recursive = T)
  # Save out to be read in later on
  openxlsx::write.xlsx(impacts,paste0("./CleanedData/MostlyImpactData/Desinventar/",iso3,"/",iso3,".xlsx"))
  # Output the safeword... TRUE!
  return(T)
}

WrangleDessie<-function(iso3,forcer=T){
  iso3%<>%str_to_lower()
  
  chk<-tryCatch(GetDessie(iso3,forcer = forcer),error=function(e) NA)
  if(is.na(chk)) return(F)
  chk<-tryCatch(ReadDessie(iso3,forcer = forcer),error=function(e) NA)
  if(is.na(chk)) return(F)
  
  return(T)
}

DesHazards<-function(Dessie){
  # Extract the list of translated Desinventar hazards
  colConv<-openxlsx::read.xlsx("./Data/Taxonomies/Desinventar_HIP.xlsx")
  # Make sure to avoid missing out!
  colConv$event%<>%str_to_lower()
  # Also check for duplicates
  colConv%<>%dplyr::select(-ISO3)%>%distinct()
  # Extract the names of the disasters
  haznams<-colConv$event[!is.na(colConv$haz_Ab)]
  # Now remove all non-relevant hazards
  Dessie%<>%mutate(event=str_to_lower(event))%>%filter(event%in%haznams)
  # Reduce the translated vector and merge
  Dessie%<>%left_join(colConv%>%dplyr::select(-c(event_en))%>%distinct(),
                      by = "event",relationship="many-to-one")
  # Remove all irrelevant hazards
  Dessie%<>%filter(!is.na(haz_type))
  
  return(Dessie)
}

# Function to produce the Excel spreadsheet that can be used to translate the hazards
SpitDesTrans<-function(Dessie){
  out<-Dessie%>%group_by(ev_ISO3s)%>%reframe(event=unique(event))
  out$event%<>%str_to_lower()
  out%<>%filter(!duplicated(out$event))
  # Try to automatically translate them using DeepL
  # Find out which languages are available
  deep_langs<-deeplr::available_languages2(auth_key = deepl_token)
  # Translate it!
  colConv<-do.call(rbind,lapply(1:nrow(out),function(i){
    trtr<-tryCatch(deeplr::translate2(out$event[i],auth_key = deepl_token,get_detect = T),error=function(e) NULL)
    if(is.null(trtr)) return(data.frame(ev_ISO3s=out$ev_ISO3s[i],event=out$event[i],event_en=NA,src_lang=NA))
    # Output the expected language from DeepL
    src_lang<-deep_langs$name[deep_langs$language==trtr$source_lang]
    # Output it
    data.frame(ev_ISO3s=out$ev_ISO3s[i],event=out$event[i],event_en=trtr$translation,src_lang=src_lang)
  }))
  # Save it out
  openxlsx::write.xlsx(colConv,"./Taxonomies/ConvertFromDatabases/Desinventar_HIP.xlsx")
  
  return(colConv)
}

PostModTransies<-function(colConv){
  # General Hazard Definitions 
  colConv$haz_Ab[grepl("earthquake",colConv$event_en,ignore.case = T)]<-"EQ"
  colConv$haz_Ab[grepl("flood",colConv$event_en,ignore.case = T)]<-"FL"
  colConv$haz_Ab[grepl("inundation",colConv$event_en,ignore.case = T)]<-"FL"
  colConv$haz_Ab[grepl("tsunami",colConv$event_en,ignore.case = T)]<-"TS"
  colConv$haz_Ab[grepl("tidal wave",colConv$event_en,ignore.case = T)]<-"TS"
  colConv$haz_Ab[grepl("rain",colConv$event_en,ignore.case = T)]<-"ST"
  colConv$haz_Ab[grepl("storm",colConv$event_en,ignore.case = T)]<-"ST"
  colConv$haz_Ab[grepl("wind",colConv$event_en,ignore.case = T)]<-"VW"
  colConv$haz_Ab[grepl("lightning",colConv$event_en,ignore.case = T)]<-"ST"
  colConv$haz_Ab[grepl("surge",colConv$event_en,ignore.case = T)]<-"SS"
  colConv$haz_Ab[grepl("torrent",colConv$event_en,ignore.case = T)]<-"FL"
  colConv$haz_Ab[grepl("cyclone",colConv$event_en,ignore.case = T)]<-"TC"
  colConv$haz_Ab[grepl("hurricane",colConv$event_en,ignore.case = T)]<-"TC"
  colConv$haz_Ab[grepl("tornado",colConv$event_en,ignore.case = T)]<-"TC"
  colConv$haz_Ab[grepl("typhoon",colConv$event_en,ignore.case = T)]<-"TC"
  colConv$haz_Ab[grepl("heat",colConv$event_en,ignore.case = T)]<-"HW"
  colConv$haz_Ab[grepl("cold",colConv$event_en,ignore.case = T)]<-"CW"
  colConv$haz_Ab[grepl("frost",colConv$event_en,ignore.case = T)]<-"ET"
  colConv$haz_Ab[grepl("ice ",colConv$event_en,ignore.case = T)]<-"ET"
  colConv$haz_Ab[grepl("fire",colConv$event_en,ignore.case = T)]<-"WF"
  colConv$haz_Ab[grepl("eruption",colConv$event_en,ignore.case = T)]<-"VO"
  colConv$haz_Ab[grepl("volcan",colConv$event_en,ignore.case = T)]<-"VO"
  colConv$haz_Ab[grepl("lava",colConv$event_en,ignore.case = T)]<-"VO"
  colConv$haz_Ab[grepl("landslide",colConv$event_en,ignore.case = T)]<-"LS"
  colConv$haz_Ab[grepl("liquefaction",colConv$event_en,ignore.case = T)]<-"LS"
  colConv$haz_Ab[grepl("mudflow",colConv$event_en,ignore.case = T)]<-"MS"
  colConv$haz_Ab[grepl("mud flow",colConv$event_en,ignore.case = T)]<-"MS"
  colConv$haz_Ab[grepl("land slide",colConv$event_en,ignore.case = T)]<-"LS"
  colConv$haz_Ab[grepl("debris flow",colConv$event_en,ignore.case = T)]<-"LS"
  colConv$haz_Ab[grepl("rock",colConv$event_en,ignore.case = T)]<-"LS"
  colConv$haz_Ab[grepl("avalanche",colConv$event_en,ignore.case = T)]<-"AV"
  colConv$haz_Ab[grepl("drought",colConv$event_en,ignore.case = T)]<-"DR"
  colConv$haz_Ab[grepl("hail",colConv$event_en,ignore.case = T)]<-"ST"
  colConv$haz_Ab[grepl("snow",colConv$event_en,ignore.case = T)]<-"SN"
  colConv$haz_Ab[grepl("epidemic",colConv$event_en,ignore.case = T)]<-"EP"
  colConv$haz_Ab[grepl("biolog",colConv$event_en,ignore.case = T)]<-"EP"
  colConv$haz_Ab[grepl("cyclone & flood",colConv$event_en,ignore.case = T)]<-"TC:FL"
  
  # hazard Types
  colConv$haz_type[colConv$haz_Ab%in%c("FL","ST","TC","DR","ET","SN","CW","HW","SS")]<-"haztypehydromet"
  colConv$haz_type[colConv$haz_Ab%in%c("EQ","LS","TS","VO","AV")]<-"haztypegeohaz"
  colConv$haz_type[colConv$haz_Ab=="WF"]<-"haztypeenviron"
  colConv$haz_type[colConv$haz_Ab=="EP"]<-"haztypebio"
  colConv$haz_type[grepl("cyclone & flood",colConv$event_en,ignore.case = T)]<-"haztypehydromet"
  
  # Hazard clusters
  colConv$haz_cluster[colConv$haz_Ab=="DR"]<-"hazhmprecip:hazhmtemp"
  colConv$haz_cluster[colConv$haz_Ab=="FL"]<-"hazhmflood"
  colConv$haz_cluster[colConv$haz_Ab=="ST"]<-"hazhmconv:hazhmwind:hazhmpress:hazhmflood"
  colConv$haz_cluster[grepl("rain",colConv$event_en,ignore.case = T)]<-"hazhmprecip"
  colConv$haz_cluster[grepl("wind",colConv$event_en,ignore.case = T)]<-"hazhmwind,hazhmpress"
  colConv$haz_cluster[grepl("lightning",colConv$event_en,ignore.case = T)]<-"hazhmconv"
  colConv$haz_cluster[colConv$haz_Ab=="ET"]<-"hazhmtemp"
  colConv$haz_cluster[colConv$haz_Ab=="TC"]<-"hazhmwind:hazhmpress:hazhmconv:hazhmflood"
  colConv$haz_cluster[colConv$haz_Ab=="TS"]<-"hazgeoother:hazhmmarine:hazhmflood"
  colConv$haz_cluster[colConv$haz_Ab=="EQ"]<-"hazgeoseis"
  colConv$haz_cluster[colConv$haz_Ab=="VO"]<-"hazgeovolc"
  colConv$haz_cluster[colConv$haz_Ab=="WF"]<-"hazenvenvdeg"
  colConv$haz_cluster[grepl("hail",colConv$event_en,ignore.case = T)]<-"hazhmprecip"
  colConv$haz_cluster[colConv$haz_Ab=="LS"]<-"hazgeoseis:hazenvenvdeg:hazgeovolc:hazgeoother"
  colConv$haz_cluster[grepl("rock",colConv$event_en,ignore.case = T)]<-"hazhmterr"
  colConv$haz_cluster[grepl("mud",colConv$event_en,ignore.case = T)]<-"hazhmterr"
  colConv$haz_cluster[grepl("liquefaction",colConv$event_en,ignore.case = T)]<-"hazgeoseis:hazgeoother"
  colConv$haz_cluster[colConv$haz_Ab=="AV"]<-"hazhmterr"
  colConv$haz_cluster[grepl("surge",colConv$event_en,ignore.case = T)]<-"hazhmmarine:hazhmflood:hazhmwind"
  colConv$haz_cluster[grepl("tidal",colConv$event_en,ignore.case = T)]<-"hazhmmarine:hazhmflood"
  colConv$haz_cluster[grepl("coastal flood",colConv$event_en,ignore.case = T)]<-"hazhmflood:hazhmmarine"
  colConv$haz_cluster[grepl("wave",colConv$event_en,ignore.case = T)]<-"hazhmmarine:hazhmflood"
  colConv$haz_cluster[grepl("hail",colConv$event_en,ignore.case = T)]<-"hazhmprecip"
  colConv$haz_cluster[grepl("tropical storm",colConv$event_en,ignore.case = T)]<-"hazhmwind"
  colConv$haz_cluster[grepl("convective storm",colConv$event_en,ignore.case = T)]<-"hazhmconv"
  colConv$haz_cluster[grepl("electric",colConv$event_en,ignore.case = T)]<-"hazhmconv"
  colConv$haz_cluster[grepl("cold wave",colConv$event_en,ignore.case = T)]<-"hazhmtemp"
  
  # Specific Hazards
  # Avalanche
  colConv$haz_spec[colConv$haz_Ab=="AV"]<-"MH0050"
  # Cold wave
  colConv$haz_spec[colConv$haz_Ab=="CW"]<-"MH0040"
  # Drought
  colConv$haz_spec[colConv$haz_Ab=="DR"]<-"MH0035"
  # Dzud
  colConv$haz_spec[colConv$haz_Ab=="DZ"]<-"MH0041"
  # Extreme precipitation
  colConv$haz_spec[colConv$haz_Ab=="EP"]<-"MH0006:MH0007:TL0046"
  # Earthquakes
  colConv$haz_spec[colConv$haz_Ab=="EQ"]<-"GH0001:GH0002"
  # Extreme temperatures
  colConv$haz_spec[colConv$haz_Ab=="ET"]<-"MH0047:MH0040:MH0042:MH0043:MH0037:MH0038:MH0039"
  # Floods
  colConv$haz_spec[colConv$haz_Ab=="FL"]<-"MH0004:MH0005:MH0006:MH0007:MH0008:MH0012"
  # Heatwave
  colConv$haz_spec[colConv$haz_Ab=="HW"]<-"MH0047"
  # Landslide
  colConv$haz_spec[colConv$haz_Ab=="LS"]<-"GH0007:GH0005:MH0051:MH0052"
  # Mudslide
  colConv$haz_spec[colConv$haz_Ab=="MS"]<-"MH0051"
  # Snow
  colConv$haz_spec[colConv$haz_Ab=="SN"]<-"MH0038"
  # Storm surge
  colConv$haz_spec[colConv$haz_Ab=="SS"]<-"MH0027"
  # Storm
  colConv$haz_spec[colConv$haz_Ab=="ST"]<-"MH0059:MH0001:MH0002:MH0003:MH0027:MH0054:MH0060"
  # Tropical cyclone
  colConv$haz_spec[colConv$haz_Ab=="TC"]<-"MH0057:MH0059"
  # Tsunami
  colConv$haz_spec[colConv$haz_Ab=="TS"]<-"GH0006"
  # Volcanic activity
  colConv$haz_spec[colConv$haz_Ab=="VO"]<-"GH0012:GH0013:GH0009:GH0010"
  # Violent winds
  colConv$haz_spec[colConv$haz_Ab=="VW"]<-"MH0054:MH0060"
  # Wildfire
  colConv$haz_spec[colConv$haz_Ab=="WF"]<-"EN0013"
  # Tropical cyclone and flooding
  colConv$haz_spec[colConv$haz_Ab=="TC:FL"]<-"MH0057:MH0059:MH0004:MH0005:MH0006:MH0007:MH0008:MH0012"
  
  # Save it out
  openxlsx::write.xlsx(colConv,"./Taxonomies/ConvertFromDatabases/Desinventar_HIP.xlsx")
  
  return(colConv)
}

GetEachDesinventar<-function(ISO3,forcer=F){
  # Desinventar is in lower case ISO3C code
  ISO3%<>%str_to_lower()
  # Get the translated names of the Desinventar countries
  DesIsos<-GetDessieISOs()
  # Find the impact files
  filer<-paste0("./CleanedData/MostlyImpactData/Desinventar/",ISO3,"/",ISO3,".xlsx")
  # Check if the file exists
  if(!file.exists(filer) & forcer) {
    # If not, try to extract it from the database
    if(!WrangleDessie(ISO3,forcer = T)) {
      print(paste0("Desinventar data not possible for ", ISO3))
      return(data.frame())
    }
  }
  # Load the data
  out<-openxlsx::read.xlsx(filer)
  # error if there is no data
  if(nrow(out)==0) stop(paste0("no Desinventar data for ISO code ",str_to_upper(ISO3)))
  # Add the country ISO3 code to the data
  out$ISO3<-DesIsos$actualiso[DesIsos$isos==ISO3]
  # The Desinventar database has many entries per single event, so we take the most recent estimate
  impies<-impies[nrow(impies):1,]#%>%filter(imp_value>0)
  # Find the duplicated elements
  inds<-impies%>%dplyr::select(imp_sub_ID)%>%duplicated()
  
  return(impies[!inds,])
}

GetDesinventar<-function(forcer=T, ISO3s=NULL){
  if(!forcer & file.exists("./Data/RawData/Desinventar.RData")) return(readRDS("./Data/RawData/Desinventar.RData"))
  # Only certain countries have Desinventar databases
  if(is.null(ISO3s)) ISO3s<-GetDessieISOs()$isos
  # Download the most recent data from Desinventar
  if(forcer) {
    wran<-unname(unlist(parallel::mclapply(ISO3s,function(is) WrangleDessie(is,forcer = forcer),mc.cores=min(10,ncores))))
    if(any(!wran)) print("countries that didn't work = ",ISO3s[!wran],collapse(" ,"))
    ISO3s<-ISO3s[wran]
  }
  # 
  fulldes<-do.call(dplyr::bind_rows,Filter(isemptylist,(lapply(ISO3s,function(iso){
    print(str_to_upper(iso))
    # Extract raw Dessie data
    Dessie<-tryCatch(GetEachDesinventar(iso),error=function(e) NULL)
    if(is.null(Dessie)) return(list())
    # Get rid of repeated entries
    Dessie%<>%distinct()%>%arrange(sdate)%>%
      filter(!is.na(haz_spec) & !is.na(imp_value) & imp_value>0)
  }))))
  # Write it out just for keep-sake
  write(jsonlite::toJSON(dMonty,pretty = T,auto_unbox=T,na = 'null'),
        paste0("./CleanedData/MostlyImpactData/UNDRR/Desinventar_",Sys.Date(),".json"))
  
  #@@@@@ Checks and validation @@@@@#
  dMonty%<>%checkMonty()
  
  return(dMonty)
}

# ISO3s<-names(sort(table(readRDS("./CleanedData/MostlyImpactData/Desinventar/fullDessie_20240729.RData")$ev_ISO3s)))
# isis<-str_split(list.files("./CleanedData/MostlyHazardData/UNDRR/"),"_",simplify = T)[,2]
# 
# ISO3s<-ISO3s[!ISO3s%in%isis]
# 
# dMonty<-MergeMonty(lapply(list.files("./CleanedData/MostlyImpactData/UNDRR/",full.names = T),
#                           function(x) checkMonty(jsonlite::fromJSON(x))))

# 
# DesIsos<-list.dirs("./RawData/MostlyImpactData/Desinventar/",recursive = F,full.names = F)
# 
# impies<-parallel::mclapply(mc.cores = 10, DesIsos, function(iso3) {
# 
#   chk<-GetDessie(iso3,T)
# 
#   xmlly<-xml2::as_list(xml2::read_xml(paste0("./RawData/MostlyImpactData/Desinventar/",iso3,"/DI_export_",iso3,".xml")))
# 
#   impacts<-do.call(dplyr::bind_rows,lapply(seq_along(xmlly$DESINVENTAR$fichas),function(i){
#     return(as.data.frame(t(as.data.frame(unlist(xmlly$DESINVENTAR$fichas[[i]])))))
#   })) %>% distinct(); rownames(impacts)<-NULL
# 
#   return(impacts)
# })



# DesIsos<-list.dirs("./RawData/MostlyImpactData/Desinventar/",recursive = F,full.names = F)
# 
# desADM<-parallel::mclapply(mc.cores = 10, Des, function(iso3) {
# 
#   chk<-GetDessie(iso3,T)
# 
#   xmlly<-xml2::as_list(xml2::read_xml(paste0("./RawData/MostlyImpactData/Desinventar/",iso3,"/DI_export_",iso3,".xml")))
# 
#   regions<-do.call(dplyr::bind_rows,lapply(seq_along(xmlly$DESINVENTAR$regiones),function(i){
#     return(as.data.frame(t(as.data.frame(unlist(xmlly$DESINVENTAR$regiones[[i]])))))
#   })) %>% distinct(); rownames(regions)<-NULL
# 
#   maps<-do.call(dplyr::bind_rows,lapply(seq_along(xmlly$DESINVENTAR$level_maps),function(i){
#     return(as.data.frame(t(as.data.frame(unlist(xmlly$DESINVENTAR$level_maps[[i]])))))
#   })) %>% distinct(); rownames(maps)<-NULL
#   # Remove any NAs
#   maps%<>%filter(!is.na(filename))
#   # Extract the spatial data
#   mapsout<-lapply(1:nrow(maps),function(j){
#     # Extract the file name of the shapefile for the admin boundaries
#     loccy<-str_split(maps$filename[j],"/",simplify = T); loccy<-loccy[length(loccy)]
#     # Adminboundary read-in
#     sf::st_read(paste0("./RawData/MostlyImpactData/Desinventar/",iso3,"/",loccy),quiet=T)%>%as.data.frame()%>%dplyr::select(-geometry)
#   })
#   tmp<-tryCatch(do.call(dplyr::bind_rows,mapsout),error=function(e) NA)
#   if(!is.na(tmp)) {mapsout<-tmp; rm(tmp)}
# 
#   saveRDS(list(regions=regions,mapsout=mapsout),paste0("./RawData/MostlyImpactData/Desinventar/Maps/",iso3,"_regionmaps.RData"))
# 
#   return(T)
# })


# tmp<-Dessie[nrow(Dessie):1,]%>%filter(imp_value>0)
# inds<-tmp%>%dplyr::select(imp_sub_ID)%>%duplicated()
# Dessie<-tmp[!inds,]
# saveRDS(Dessie,"./CleanedData/MostlyImpactData/Desinventar/subDessie.RData")


# chk<-sapply(DesIsos, function(is) tryCatch(GetDessie(is),error=function(e) NA),simplify = T)
# 
# chk<-sapply(DesIsos, function(is) {
#   print(paste0("Trying for country: ",is))
#   out<-tryCatch(ReadDessie(is),error=function(e) NA)
#   if(is.na(out)) {print("FAIL")} else print("SUCCESS")
#   return(out)
# },simplify = T)

# chk<-sapply(DesIsos, function(is) WrangleDessie(is),simplify = T)
# chk<-mclapply(DesIsos, function(is) WrangleDessie(str_to_lower(is)),mc.cores = 10)
# chk<-unlist(mclapply(unique(na.omit(impies$imp_ISO3s)), 
#               function(is) tryCatch(GetDessie(is,forcer=T),
#                                     error=function(e) F),
#               mc.cores = 10))
# 
# chk<-mclapply(unique(na.omit(impies$imp_ISO3s))[chk], 
#               function(is) tryCatch(ReadDessie(is,forcer=T),
#                                     error=function(e) F),
#               mc.cores = 10)

# 

# fully<-data.frame(CountryName=DesCountries[unlist(sapply(list.files("./CleanedData/SocioPoliticalData/Desinventar/"), function(st) which(DesIsos==st),simplify = T))],
#                   ISO3C=list.files("./CleanedData/SocioPoliticalData/Desinventar/"),
#                   Status="Complete", RawData="Downloaded", 
#                   ImpactData="Wrangled", ADMboundaries="Wrangled")
# # write_csv2(fully,"./CleanedData/MostlyImpactData/Desinventar/CountryStatus_Full.csv")
# fully<-read_csv2("./CleanedData/MostlyImpactData/Desinventar/CountryStatus_Full.csv")
# 
# Incomplete<-list.files("./CleanedData/MostlyImpactData/Desinventar/"); Incomplete<-Incomplete[!grepl(".csv",Incomplete)]
# Incomplete<-Incomplete[!Incomplete%in%fully$ISO3C]
#   
# Incomplete<-data.frame(CountryName=DesCountries[unlist(sapply(Incomplete, function(st) which(DesIsos==st),simplify = T))],
#                   ISO3C=Incomplete, Status="Incomplete",
#                   RawData="Downloaded", ImpactData="Wrangled",
#                   ADMboundaries="Incomplete")
# 
# # Partially complete countries
# itmp<-Incomplete$ISO3C%in%list.files("./CleanedData/SocioPoliticalData/Desinventar/") &
#   !Incomplete$ISO3C%in%fully$ISO3C
# # Extract them
# ParComp<-Incomplete[itmp,]
# Incomplete<-Incomplete[!itmp,]
# 
# # Modify the partially completed countries
# ParComp$Status<-"Complete"
# ParComp$ADMboundaries<-"Wrangled but with ADM level 2 bodging"
# 
# CurStat<-rbind(fully, ParComp, Incomplete)
# 
# write_csv2(CurStat,"./CleanedData/MostlyImpactData/Desinventar/CountryStatus_all.csv")
# 
# # 
# chk<-sapply(Incomplete$ISO3C, function(is) {
#   print(paste0("Trying for country: ",is))
#   out<-tryCatch(ReadDessie(is),error=function(e) NA)
#   if(is.na(out)) {print("FAIL")} else print("SUCCESS")
#   return(out)
# },simplify = T)













# javascript:window.location='http://www.desinventar.net/DesInventar/stats_excel.jsp?bookmark=1&countrycode=alb&maxhits=100&lang=EN&logic=AND&sortby=0&frompage=/definestats.jsp&bSum=Y&_stat=fichas.fechano,,&nlevels=1&_variables=1,fichas.muertos,fichas.heridos,fichas.desaparece,fichas.vivdest,fichas.vivafec,fichas.damnificados,fichas.afectados,fichas.reubicados,fichas.evacuados,fichas.valorus,fichas.valorloc,fichas.nescuelas,fichas.nhospitales,fichas.nhectareas,fichas.cabezas,fichas.kmvias&rndp=13180'
# 
# "https://www.desinventar.net/DesInventar/stats_spreadsheet.jsp?bookmark=1&countrycode=alb&maxhits=100&lang=EN&logic=AND&sortby=0&frompage=/definestats.jsp&bSum=Y&_stat=fichas.fechano,,&nlevels=1&_variables=1,fichas.muertos,fichas.heridos,fichas.desaparece,fichas.vivdest,fichas.vivafec,fichas.damnificados,fichas.afectados,fichas.reubicados,fichas.evacuados,fichas.valorus,fichas.valorloc,fichas.nescuelas,fichas.nhospitales,fichas.nhectareas,fichas.cabezas,fichas.kmvias&_eventos="


