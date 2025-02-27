
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
  heprod<-faostat$yield%>%left_join(faostat$item_groups,by=c("Item"))%>%
    filter(!is.na(item_grouping_f) & Year>2015)%>%
    group_by(ISO3.CODE,item_grouping_f,Year)%>%
    reframe(sumyield=sum(Yield,na.rm = T))%>%
    group_by(ISO3.CODE,item_grouping_f)%>%
    reframe(avyield=mean(sumyield,na.rm=T))
  # Calculate the production proportion between different commodity groups, per country
  avprod<-faostat$Prod%>%dplyr::select(-any_of(c("item_grouping_f")))%>%
    left_join(faostat$item_groups,by=c("Item"))%>%
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
  
  
  
  
  pricy<-faostat$price
  
  
  
  
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

# Check through the values to be input to stan
Check4Stan<-function(fdf){
  # Which elements have NAs?
  nannies<-unlist(lapply(seq_along(fdf),function(i) any(is.na(fdf[[i]]))))
  if(sum(nannies)>0 & any(!names(fdf)[nannies]%in%c("ev_id"))) warning(paste0("Stan data input check: NA values found in ",names(fdf)[nannies]))
  inds<-which(names(fdf)%in%c("n_t","n_isos","n_haz","n_com"))
  ck<-unlist(fdf[inds])
  if(any(ck)<=0) stop(paste0("Stan data input check: zero dimensionality found one of : ",paste0(c("n_t","n_isos","n_haz","n_com"),collapse = ", ")))
  if(fdf$n_t!=length(fdf$time)) stop("Stan data input check: time dimensionality or time variable incorrectly created")
  if(fdf$n_isos!=dim(fdf$y)[1]) stop("Stan data input check: ISO dimensionality or ISO column incorrectly created")
  if(fdf$n_haz!=length(unique(fdf$htype))) stop("Stan data input check: hazard dimensionality or hazard type vector incorrectly created")
  if(fdf$n_com!=dim(fdf$y)[3]) stop("Stan data input check: commodity dimensionality or commodity column incorrectly created")
  if(any(is.na(fdf$n_dis)) | any(fdf$n_dis<0)) stop("Stan data input check: disaster dimensionality incorrectly created")
  if(!all(nchar(fdf$isos)==3)) stop("Stan data input check: ISO3 codes which are not compatible (not ISO3C-codes)")
  if(any(fdf$y<0)) stop("Stan data input check: negative commodity data")
  if(any(!fdf$flag%in%c(0,1)))  {
    warning("Stan data input check: flag array not in binary format, check values")
    fdf$flag[!fdf$flag%in%c(0,1)]<-0
  }
  if(any(fdf$ts<0)){
    warning("Stan data input check: negative ts values")
    fdf$ts[fdf$ts<0]<-0
  }  
  if(any(fdf$tf<0)){
    warning("Stan data input check: negative tf values")
    fdf$tf[fdf$tf<0]<-0
  }  
  if(any(fdf$hazdur<0)){
    warning("Stan data input check: negative hazard duration (hazdur) values")
    fdf$hazdur[fdf$hazdur<0]<-0
  }
  if(any(!fdf$htype%in%0:fdf$n_haz)){
    warning("CORRECTING! Stan data input check: incorrect labelling of hazard types")
    fdf$htype[!fdf$htype%in%0:fdf$n_haz]<-0
  }
  if(any(fdf$iprox<0)){
    warning("CORRECTING! Stan data input check: negative iprox values, setting to zero")
    fdf$iprox[fdf$iprox<0]<-0
  }
  if(any(fdf$mu_dis<0)){
    warning("CORRECTING! Stan data input check: negative mu_dis values, setting to zero")
    fdf$mu_dis[fdf$mu_dis<0]<-0
  }
  if(any(fdf$sig_dis<0)){
    warning("CORRECTING! Stan data input check: negative sig_dis values, setting to 1e-6")
    fdf$sig_dis[fdf$sig_dis<0]<-1e-6
  }
  if(any(fdf$mu_AR1<0)){
    warning("CORRECTING! Stan data input check: negative mu_AR1 values, setting to zero")
    fdf$mu_AR1[fdf$mu_AR1<0]<-0
  }
  if(any(fdf$sig_AR1<0)){
    warning("CORRECTING! Stan data input check: negative sig_AR1 values, setting to 1e-6")
    fdf$sig_AR1[fdf$sig_AR1<0]<-1e-6
  }
  return(fdf)
}

# Once disaster severity has been predicted, generate all the data we need 
Prepare4Model<-function(faostat,sevvies,syear=1991,fyear=2023){
  # Some dimensions
  n_t <- fyear-syear+1L        # Number of years
  n_dis <- 30 # number of disasters per country
  n_com <- length(unique(faostat$item_groups$item_grouping_f))
  # Make sure the data covers the correct range
  faostat$yield%<>%filter(Year>=syear & Year<=fyear)
  faostat$Area%<>%filter(Year>=syear & Year<=fyear)
  faostat$Prod%<>%filter(Year>=syear & Year<=fyear)
  sevvies%<>%filter(year>=syear & year<=fyear & ISO3 %in% unique(faostat$Prod$ISO3.CODE))%>%
    mutate(mu=case_when(is.na(mu) | is.na(sd) | is.infinite(mu) | is.infinite(sd) ~ 0, T ~ mu),
           sd=case_when(is.na(mu) | is.na(sd) | is.infinite(mu) | is.infinite(sd) ~ 1e-6, T ~ sd))
  # Normalise the disaster severity globally to ensure the mu+3*sigma is not larger than the median production
  # (this implies that it is very unlikely that a disaster will occur that will result in losing more than 50% of the production)
  scalefac<-median(log(faostat$Prod$Production),na.rm = T)/median(sevvies$mu+sevvies$sd*3,na.rm = T)
  sevvies$mu<-sevvies$mu*scalefac
  # Prepare the time/year related variables
  sevvies %<>%
    mutate(sdate = as.Date(sdate),
           fdate = as.Date(fdate),
           sy = year(sdate),                     
           ey = year(fdate),                   
           s_frac = (yday(sdate) - 1) / 365,        
           e_frac = yday(fdate) / 365,             
           duration_years = as.numeric(fdate - sdate) / 365,  
           endt = sy + s_frac + duration_years)%>%
    arrange(ISO3, sdate)
  # Calculate average disaster severity per hazard then use this to normalise the weighting in the top-n most severe
  weights<-sevvies%>%group_by(haz_grp)%>%reframe(hazweight=1/mean(mu,na.rm=T))%>%
    mutate(hazweight=hazweight/max(hazweight,na.rm=T),
           hazweight=case_when(is.infinite(hazweight) ~ 1, T ~ hazweight))
  # Reduce the number of disasters so that it is only the top-n most severe
  disnos<-sevvies%>%
    left_join(weights,by="haz_grp")%>%
    group_by(ISO3,disno)%>%
    reframe(mu=log(sum(exp(mu))),
            hazweight=mean(hazweight),
            disno=unique(disno))%>%
    group_by(ISO3)%>%
    mutate(weights=pmax(mu,0)*hazweight,
           weights=weights/max(weights,na.rm=T))%>%
    ungroup()%>%mutate(weights=case_when(is.infinite(weights)~1,T~weights))%>%
    arrange(desc(weights))%>%
    group_by(ISO3)%>%slice(1:pmin(n_dis,n()))
  # Filter the disaster events
  redsev<-sevvies%>%filter(disno%in%disnos$disno)%>%
    mutate(haz_grp_int=as.integer(as.factor(haz_grp)))
  # Production data from faostat
  prod <- faostat$Prod%>%dplyr::select(-any_of(c("item_grouping_f")))%>%
    left_join(faostat$item_groups,by=c("Item"))%>%
    filter(!is.na(item_grouping_f) & Year>=syear & Year<=fyear)%>%
    group_by(ISO3.CODE,item_grouping_f,Year)%>%
    reframe(Prod=sum(Production,na.rm = T))%>%mutate(Year=as.integer(Year))%>%
    arrange(item_grouping_f)
  # Ensure all commodities exist for each country-year combination
  prod <- prod %>%
    complete(ISO3.CODE, item_grouping_f, Year, fill = list(Prod = 0)) %>%  # Fill missing commodities with zero
    arrange(ISO3.CODE, item_grouping_f, Year)
  # Extract country ISO3C codes
  isos <- unique(prod$ISO3.CODE)
  # Filter just in case
  redsev%<>%filter(ISO3%in%isos)
  # Dimensions declaration
  n_isos <- length(isos)      # Number of countries
  n_haz <- length(unique(redsev$haz_grp))       # Number of hazard types
  # Timeline
  time <- seq(1, n_t)
  yrs <- seq(syear,fyear)
  # Hazard types per disaster 
  htype <- array(0, dim = c(n_isos, n_dis)) 
  # Disaster occurrence flag
  flag <- array(0, dim = c(n_isos, n_t, n_dis),
                dimnames = list(isos, as.character(yrs), NULL)) 
  # Hazard duration per year
  hazdur <- array(0, dim = c(n_isos, n_t, n_dis),
                  dimnames = list(isos, as.character(yrs), NULL)) 
  # Disaster duration start to end window per year
  ts <- array(0, dim = c(n_isos, n_t, n_dis),
              dimnames = list(isos, as.character(yrs), NULL)) 
  tf <- array(0, dim = c(n_isos, n_t, n_dis),
              dimnames = list(isos, as.character(yrs), NULL)) 
  # Disaster event_id 
  ev_id <- array(NA, dim = c(n_isos, n_dis)) 
  # Disaster severity
  iprox <- array(0, dim = c(n_isos, n_dis, n_com)) 
  # AR1-related variables
  mu_AR1 <- array(0, dim = c(n_isos, n_com)) 
  sig_AR1 <- array(0, dim = c(n_isos, n_com)) 
  # Disaster severity sampling (replaces iprox)
  mu_dis <- array(0, dim = c(n_isos, n_dis, n_com)) 
  sig_dis <- array(0, dim = c(n_isos, n_dis, n_com)) 
  # Commodity data
  y <- array(0, dim = c(n_isos, n_t, n_com)) 
  # Create an array of the number of disasters per country
  n_dis_v<-integer(n_t)
  # Which commodities are we covering?
  commods<-sort(unique(prod$item_grouping_f))
  # Iterate over all countries
  for(j in 1:length(isos)){
    is<-isos[j]
    # All years in the data
    for(t in time){
      # All-year commodities data
      y[j,t, ] <- pull(arrange(prod[prod$ISO3.CODE==is & prod$Year==yrs[t],],item_grouping_f),Prod)
    }
    # Filter only the relevant disaster severity records
    isosev<-redsev%>%filter(ISO3==is)
    # Compute AR(1) estimates with pre-filled zeros
    armod <- prod %>% filter(ISO3.CODE == is) %>% group_by(item_grouping_f) %>%
      reframe(AR1 = ifelse(n() > 1, Rfast::ar1(Prod)[["phi"]], 0),  # If only one value, return 0 for AR1
              sigAR1 = ifelse(n() > 1, sqrt(Rfast::ar1(Prod)[["sigma"]]), 1e-9))%>%  # If only one value, return very small value
      right_join(data.frame(item_grouping_f = commods), by = "item_grouping_f") %>%
      mutate(AR1 = replace_na(AR1, 0), # Replace NA in AR1 with 0
             sigAR1 = replace_na(sigAR1, 1e-9))%>% # Replace NA in sigAR1 with really small but non-zero value
      arrange(item_grouping_f)
    # Convert to matrix format
    armod%<>%arrange(item_grouping_f)%>%dplyr::select(2:3)%>%as.matrix()
    # Store it
    mu_AR1[j,] <- armod[,1]
    sig_AR1[j,] <- armod[,2]
    # if no disasters are present in this country, add empty values
    if(nrow(isosev)!=0){
      # This vector helps speed up the calculations in the stan model
      n_dis_v[j]<-length(unique(isosev$disno))
      # Loop over commodities
      for(k in 1:length(commods)){
        # Which commodity are we referring to?
        ic<-commods[k]
        # Filter
        comsev<-isosev%>%filter(item_grouping_f==ic)
        # Add the data
        if(nrow(comsev)==0){
          # Disaster severity
          iprox[j,1:n_dis_v[j], k] <- 0
          # Disaster severity for the sampling model
          mu_dis[j,1:n_dis_v[j], k] <- 0
          sig_dis[j,1:n_dis_v[j], k] <- 1e-9
        } else {
          # Each disaster, per country
          ev_id[j,1:n_dis_v[j]] <- comsev$disno[1:n_dis_v[j]]
          # Hazard type
          htype[j,1:n_dis_v[j]]<-comsev$haz_grp_int[1:n_dis_v[j]]
          # Disaster severity
          iprox[j,1:n_dis_v[j], k] <- comsev$mu[1:n_dis_v[j]]
          # Disaster severity for the sampling model
          mu_dis[j,1:n_dis_v[j], k] <- comsev$mu[1:n_dis_v[j]]
          sig_dis[j,1:n_dis_v[j], k] <- comsev$sd[1:n_dis_v[j]]
        }
      }
    } else {
      n_dis_v[j] <- 0
      ev_id[j,1:n_dis_v[j]] <- NA
      htype[j,1:n_dis_v[j]] <- 0
      iprox[j,1:n_dis_v[j], ] <- 0
      mu_dis[j,1:n_dis_v[j], ] <- 0 
      sig_dis[j,1:n_dis_v[j], ] <- 1e-6
    }
  }
  # Create a row index that labels each country's disasters for the matrix
  redsev%<>%dplyr::select(-c(item_grouping_f,mu,sd))%>%
    distinct()%>%group_by(ISO3)%>%mutate(evvie = 1:n())%>%ungroup()
  # distinct(across(-item_grouping_f), .keep_all = TRUE)%>%
  # Now for the awkward disaster variables
  for(i in 1:nrow(redsev)){
    iso <- redsev$ISO3[i]
    event <- redsev$evvie[i]
    sy <- redsev$sy[i]      
    ey <- redsev$ey[i]      
    sfrac <- redsev$s_frac[i]
    efrac <- redsev$e_frac[i]
    duration_years <- redsev$duration_years[i]
    endt <- redsev$endt[i]
    if(efrac>1) ey<-floor(endt)
    # Iterate over the years the disaster was present or the post-disaster years
    for(t in sy:fyear){
      t_chr <- as.character(t)
      # Create the flag variable to indicate whether the disaster contributes to the commodity change of a given year
      flag[iso, t_chr, event] <- 1
      # During the year the hazard occurs, ensure to split between the hazard-duration impact and the post-disaster-decay impact
      if(t == sy){ 
        # If the hazard duration is entirely within one year
        if((1 - sfrac) >= duration_years){
          ts[iso, t_chr, event] <- 0
          tf[iso, t_chr, event] <- 1 - sfrac - duration_years
          # If the hazard duration lasts more than a year: no contribution to dsev as the contribution comes from the hazard duration element
        } else { 
          ts[iso, t_chr, event] <- 0
          tf[iso, t_chr, event] <- 0
        }
        # Hazard duration information only transmitted to the model during the hazard year
        hazdur[iso, t_chr, event] <- duration_years
        # intermediate years where hazard is still active
      } else if(t < ey){      
        ts[iso, t_chr, event] <- 1
        tf[iso, t_chr, event] <- 1
        # Final year: Hazard ends sometime in the year
      } else if(t==ey){
        ts[iso, t_chr, event] <- 0
        tf[iso, t_chr, event] <- t+1-endt
        # After the hazard is over, calculate the remainder of the disaster decay times
      } else { 
        ts[iso, t_chr, event] <- t - endt
        tf[iso, t_chr, event] <- t + 1 - endt
      }
      if(any(ts<0)|any(tf<0)) stop(paste0("Issues with time variables: check country ",iso," for sevvies row number ",i))
    }
  }
  
  warning("You also want the price information here so that stan can calculate the losses in USD")
  # Generate the list for stan
  fdf<-list(n_t = n_t,
            n_isos = n_isos,
            n_dis = n_dis_v,
            n_haz = n_haz,
            n_com = n_com,
            time = time,
            isos = isos,
            y = y,  
            flag = flag,
            ts = ts,
            tf = tf,
            hazdur = hazdur,
            htype = htype,
            ev_id = ev_id,
            iprox = iprox,
            mu_AR1 = mu_AR1,
            sig_AR1 = sig_AR1,
            mu_dis=mu_dis,
            sig_dis=sig_dis)
  # Check through the list!
  fdf%>%Check4Stan()
}

