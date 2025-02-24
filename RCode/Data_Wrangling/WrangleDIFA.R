
# Which disaster datasets do we want?
GetDisaster<-function(syear=1990,fyear=NULL){
  # Set final year to current year
  if(is.null(fyear)) fyear<-AsYear(Sys.Date())
  # EM-DAT
  emdat<-API_EMDAT(syear=syear,fyear=fyear)%>%distinct()%>%arrange(sdate)%>%filter(!is.na(haz_Ab))
  # Desinventar
  dessie<-GetDesinventar(forcer=F)%>%filter(!is.na(haz_Ab))%>%distinct()
  # UCDP conflict dataset
  ucdp<-GetUCDP()
  
  return(list(emdat=dplyr::bind_rows(emdat,ucdp),dessie=dessie))
}

# normalise the population-, GDP- and area-related variables by national totals
NormaliseImpacts<-function(df,wb){
  # First make sure that any empty values are not normalised
  wb%<>%mutate_at(c("GDP","population","livestock","surfarea"),
                  function(x) {x[x<=0]<-NA; return(x)})
  print(paste0("Number of countries with NAs = ",paste0(sapply(3:ncol(wb),function(i) {paste0(names(wb)[i],"=",length(unique(wb$ISO3[is.na(wb[,i])])))}),collapse=", ")))
  print(paste0("Number of database countries not in wb = ",sum(!unique(df$ISO3)%in%unique(wb$ISO3))," (",paste0(unique(df$ISO3)[!unique(df$ISO3)%in%wb$ISO3],collapse = ","),")"))
  # Temporary year-wise join
  df$year<-AsYear(df$sdate)
  # Left join the World Bank data
  df%<>%left_join(wb,by = join_by(ISO3==ISO3,year==year), relationship="many-to-one")
  # Normalise the population-related & GDP-related impact types
  df%<>%mutate(norm_deaths=deaths/population, 
               norm_affected=affected/population,
               norm_cost=cost/GDP)
  # Normalise the area-related impact types, if they exist!
  if(!is.null(df$crops)) df%<>%mutate(norm_crops=crops/surfarea)
  # Normalise the livestock-related impact types, if they exist!
  if(!is.null(df$cattle)) df%<>%mutate(norm_cattle=cattle/livestock)
  
  return(df)
}

# Add the associated region to the country, for any dataframe df 
AddRegion<-function(df,isoreg,region="unregion"){
  # Select only the relevant region and income groups
  isoreg%<>%dplyr::select(all_of(c("ISO3",region,"incomecomb")))
  colnames(isoreg)[2:3]<-c("region","incomegrp")
  # Merge
  df%<>%left_join(isoreg,by = join_by(ISO3==ISO3), relationship="many-to-one")
  # Replace NAs
  df$region[is.na(df$region)]<-"Other"
  
  return(df)
}

# This is where we choose which hazards are grouped with which others... big assumptions but it only needs to be vague
hazgrp<-c(
  "CW"="ET",
  "DR"="DR",
  "WF"="WF",
  "FL"="FL",
  "ST"="ST",
  "TC"="ST",
  "EQ"="EQ",
  "VO"="WF",
  "AV"="LS",
  "LS"="LS",
  "HW"="ET",
  "TS"="ST",
  "ET"="ET",
  "EP"="FL",
  "VW"="ST",
  "SN"="LS",
  "SS"="ST",
  "MS"="LS",
  "TC:FL"="ST",
  "CF"="CF",
  "DZ"="ET"
)
# Group the hazards together, based on the data and 'expert' judgment
GroupHazs<-function(df,hazgrp=hazgrp){
  # First make sure we drop all events that we don't include in the analysis
  df%<>%filter(haz_Ab%in%names(hazgrp))
  # Create the grouping variable
  df$haz_grp<-unname(hazgrp[df$haz_Ab])
  # Remove NA events
  df%>%filter(!is.na(haz_grp))
}

# For the data normalisations, we don't have anything to normalise the Desinventar cattle data to, so calculate it from FAOSTAT
AddLivestock<-function(wb,faostat){
  # Generate the livestock data from FAOSTAT production
  livestock<-faostat$Prod%>%
    filter(item_grouping_2=="Livestock_products")%>%
    group_by(ISO3.CODE,Year)%>%
    reframe(livestock=sum(Production,na.rm = T),Year=as.integer(Year))%>%
    ungroup()%>%distinct()
  # Add it!
  wb%>%left_join(livestock,by=join_by(ISO3==ISO3.CODE,year==Year))
}

ConvHe2Tonnes<-function(sevvies,faostat){
  
}

# Prepare the DIFA data to have it in the correct format for modelling
PrepDIFA<-function(difa){
  # Normalise both the EM-DAT and Desinventar datasets
  difa$dissie$emdat%<>%NormaliseImpacts(difa$wb)
  difa$dissie$dessie%<>%NormaliseImpacts(difa$wb)
  # Add the region features
  difa$dissie$emdat%<>%AddRegion(difa$isoreg,region="unregion")
  difa$dissie$dessie%<>%AddRegion(difa$isoreg,region="unregion")
  # Group the hazards
  difa$dissie$emdat%<>%GroupHazs(hazgrp)
  difa$dissie$dessie%<>%GroupHazs(hazgrp)
  # Get rid of the elements we don't need anymore
  difa$wb<-difa$isoreg<-NULL
  difa$dissie$emdat%<>%filter(year>1989 & !is.na(haz_grp) & !is.na(ISO3))
  difa$dissie$dessie%<>%filter(year>1989 & !is.na(haz_grp) & !is.na(ISO3))
  
  return(difa)
}

# Wrangle the data but don't yet merge it
getData<-function(syear=1990,fyear=NULL){
  # Set final year to current year
  if(is.null(fyear)) fyear<-AsYear(Sys.Date())
  # Disasters (EM-DAT and Desinventar) 
  dissie<-GetDisaster(syear=syear,fyear=fyear)
  # Get yield from FAOSTAT
  faostat<-GetFAOSTAT_All(syear=syear,fyear=fyear)
  # Get yearly population and GDP data from World Bank to use in normalisations
  wb<-GetWorldBank(syear=syear,fyear=fyear)
  # Add livestock to the wb data from FAOSTAT
  wb%<>%AddLivestock(faostat)
  # Add country-region dataset
  isoreg<-GetISOregion()
  
  # Get the data into the right format and merge some of the dataframes
  list(dissie=dissie,faostat=faostat,wb=wb,isoreg=isoreg)%>%
    PrepDIFA()
}



