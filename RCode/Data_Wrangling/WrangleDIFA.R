
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
  wb%<>%mutate_at(c("GDP","population","surfarea"),
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

ConvHe2Tonnes<-function(sevvies,faostat){
  # Extract the country-wise ~10-year average yield per commodity group
  heprod<-difa$faostat$yield%>%left_join(difa$faostat$item_groups,by=c("Item"))%>%
    filter(!is.na(item_grouping_f) & Year>2015)%>%
    group_by(ISO3.CODE,item_grouping_f,Year)%>%
    reframe(sumyield=sum(Yield,na.rm = T))%>%
    group_by(ISO3.CODE,item_grouping_f)%>%
    reframe(avyield=mean(sumyield,na.rm=T))
  # Calculate the production proportion between different commodity groups, per country
  avprod<-difa$faostat$Prod%>%dplyr::select(-any_of(c("item_grouping_f")))%>%
    left_join(difa$faostat$item_groups,by=c("Item"))%>%
    filter(!is.na(item_grouping_f) & Year>2015)%>%
    group_by(ISO3.CODE,item_grouping_f,Year)%>%
    reframe(sumprod=sum(Production,na.rm = T))%>%
    group_by(ISO3.CODE,item_grouping_f)%>%
    reframe(avprod=mean(sumprod,na.rm=T))%>%
    group_by(ISO3.CODE)%>%
    reframe(item_grouping_f=item_grouping_f,
            propprod=avprod/sum(avprod))
  # Merge them
  heprod%<>%left_join(avprod,by=c("ISO3.CODE","item_grouping_f")); rm(avprod)
  # Add price per tonne
  
  
  
  
  pricy<-difa$faostat$price
  
  
  
  
  # Based on expected losses in hectares, convert to expected production losses
  sevvies%>%filter(!is.na(mu) & !is.na(sd) & dep=="crops")%>%dplyr::select(-dep)%>%
    left_join(heprod,by=join_by(ISO3==ISO3.CODE),
              relationship = "many-to-many")%>%
    mutate(mu=log(exp(mu)*propprod*avyield),
           sd=log(exp(sd)*propprod*avyield))%>%
    dplyr::select(-any_of(c("avyield","propprod","proportion")))
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
  # Add country-region dataset
  isoreg<-GetISOregion()
  
  # Get the data into the right format and merge some of the dataframes
  list(dissie=dissie,faostat=faostat,wb=wb,isoreg=isoreg)%>%
    PrepDIFA()
}



