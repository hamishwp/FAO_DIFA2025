results_df<-data.frame()
for (mxie in c(1,3,5,10,15,30)){
  for (llie in c("m_likelihood_hazdur","m_likelihood","m_likelihood_bin")){
    mcmle<-TrainModel_MCMLE(fdf, samp = 30000, cpus = 30, LL = llie, mxdis = mxie)
    ressies<-compute_weighted_stats(mcmle)
    ressies%<>%mutate(model=llie,mxdis=mxie, q95LL=quantile(mcmle$LL,probs = 0.95))
    results_df%<>%rbind(ressies)
  }
}

fdf_sim<-fdf
fdf_sim$lny<-simulate_fdf(fdf, params)
mcmle_sim<-TrainModel_MCMLE(fdf_sim, samp = 30000, cpus = 40, LL = "m_likelihood_hazdur")
ressies<-compute_weighted_stats(mcmle_sim)
ressies
mcmle<-TrainModel_MCMLE(fdf, samp = 30000, cpus = 40, LL = "m_likelihood_hazdur")
ressies<-compute_weighted_stats(mcmle)
ressies

betas<-rbind(mcmle%>%filter(LL > quantile(LL, 0.75))%>%dplyr::select(beta_dis,LL)%>%mutate(data="Real"),
             mcmle_sim%>%filter(LL > quantile(LL, 0.75))%>%dplyr::select(beta_dis,LL)%>%mutate(data="Simulated"))

p<-betas%>%ggplot()+geom_point(aes(beta_dis,LL,colour=data))+
  geom_vline(xintercept = 0,colour="black")+facet_wrap(~data)+
  ylim(c(-1.5e15,0))+xlim(c(-5,5))+
  xlab("Beta - Disaster Severity")+ylab("Likelihood (Log)");p

ggsave("./Plots/Beta_dis_realVsSim.png",p,height=5,width=8)

convert_results_to_params <- function(results_df, fdf, metric="w_mean") {
  # Helper function to extract base variable name (e.g., "hsev" from "hsev[1]")
  extract_var_name <- function(var) sub("\\[.*\\]", "", var)
  
  # Helper function to extract index from variable (e.g., 1 from "hsev[1]")
  extract_index <- function(var) {
    idx <- str_split(str_split(var,"\\[",simplify = T)[2],"\\]",simplify = T)[1]
    if (is.na(idx)) return(NA_integer_)  # Return NA if no index
    as.integer(idx)
  }
  
  # Split results_df into list of base variables
  results_df$base_var <- sapply(results_df$variable, extract_var_name)
  results_df$index <- sapply(results_df$variable, extract_index)
  
  # Create empty params list with correct dimensions
  params <- list(
    beta_dur = NA,
    beta_dis = NA,
    sigma = NA,
    beta_y1 = rep(NA, fdf$n_com),
    hsev = rep(NA, fdf$n_haz),
    isev = rep(NA, fdf$n_com)
  )
  
  # Fill scalar parameters (those without index)
  for (v in c('beta_dur', 'beta_dis', 'sigma', 'beta_y1')) {
    if (v %in% results_df$base_var) {
      params[[v]] <- results_df[results_df$base_var == v,metric]
    }
  }
  
  # Fill vector parameters (those with index)
  for (v in c('hsev', 'isev')) {
    if (v %in% results_df$base_var) {
      var_df <- subset(results_df, base_var == v & !is.na(index))
      for (i in seq_len(nrow(var_df))) {
        idx <- var_df$index[i]
        params[[v]][idx] <- var_df[i,metric]
      }
    }
  }
  
  # Fill remaining NA values with 1 if they were not provided (default)
  params$hsev[is.na(params$hsev)] <- 1
  params$isev[is.na(params$isev)] <- 1
  params$beta_y1[is.na(params$beta_y1)] <- 1
  
  return(params)
}

ressies<-readRDS("./Data/Results/results_df_MCMLE.RData")
results_df <- ressies%>%filter(model=="m_likelihood_bin" & mxdis==1)
country_region = openxlsx::read.xlsx("Data/Taxonomies/IsoContinentRegion.xlsx")%>% 
  dplyr::select(ISO.Code, Country, UN.Region, UN.Sub.Region, World.Bank.Income.Groups)%>%rename(ISO3=ISO.Code)

global<-itemres<-isores<-data.frame()
# Cycle over different number of disasters
for(i in 1:15){
  params<-readRDS("./Data/Results/params_binLL_mxdis1.RData")
  losses<-predloss_bin_par(fdf, params, mxdis=i, n_sam=1000)
  params$beta_dis <- convert_results_to_params(results_df, fdf, "w_q75")$beta_dis
  tmp<-predloss_bin_par(fdf, params, mxdis=i, n_sam=1000)
  losses$losses <- abind::abind(losses$losses, tmp$losses, along = 4)
  losses$all_losses <- abind::abind(losses$all_losses, tmp$all_losses, along = 2)
  params$beta_dis <- convert_results_to_params(results_df, fdf, "w_q25")$beta_dis
  tmp<-predloss_bin_par(fdf, params, mxdis=i, n_sam=1000)
  losses$losses <- abind::abind(losses$losses, tmp$losses, along = 4)
  losses$all_losses <- abind::abind(losses$all_losses, tmp$all_losses, along = 2)
  
  print(paste0("Mean: ",sum(apply(losses$all_losses,1,mean))/1e12))
  print(paste0("Q05: ",(sum(apply(losses$all_losses,1,quantile,probs=c(0.05)))/1e12)))
  print(paste0("Q95: ",(sum(apply(losses$all_losses,1,quantile,probs=c(0.95)))/1e12)))
  
  global%<>%rbind(data.frame(
    ndis=i,
    Mean=sum(apply(losses$all_losses,1,mean))/1e12,
    Q05=sum(apply(losses$all_losses,1,quantile,probs=c(0.05)))/1e12,
    Q95=sum(apply(losses$all_losses,1,quantile,probs=c(0.95)))/1e12))
  itemres%<>%rbind(data.frame(ndis=i,
                     Item_Group=fdf$commods,
                      Mean=apply(losses$all_losses,1,mean)/1e12,
                      Q05=apply(losses$all_losses,1,quantile,probs=c(0.05))/1e12,
                      Q95=apply(losses$all_losses,1,quantile,probs=c(0.95))/1e12))
  isores%<>%rbind(data.frame(ndis=i,
                     ISO3=fdf$isos,
                     Mean=sapply(1:fdf$n_isos,function(i) sum(sapply(1:fdf$n_t,function(j) sum(sapply(1:fdf$n_com,function(k) mean(losses$losses[i,j,k,]))))))/1e12,
                     Q05=sapply(1:fdf$n_isos,function(i) sum(sapply(1:fdf$n_t,function(j) sum(sapply(1:fdf$n_com,function(k) quantile(losses$losses[i,j,k,],probs = c(0.05)))))))/1e12,
                     Q95=sapply(1:fdf$n_isos,function(i) sum(sapply(1:fdf$n_t,function(j) sum(sapply(1:fdf$n_com,function(k) quantile(losses$losses[i,j,k,],probs = c(0.95)))))))/1e12)%>%
    left_join(country_region,by="ISO3",relationship="one-to-one"))
}

dis_nt<-data.frame(ndis=1:15, tdis=sapply(1:15,function(i) sum(pmin(fdf$n_dis,i))))

# Function to smooth the values
smooth_values <- function(data, x_var, y_vars, span = 0.9) {
  data_smooth <- data %>%
    mutate(
      Mean_smooth = predict(loess(Mean ~ ndis, data = data, span = span)),
      Q05_smooth = predict(loess(Q05 ~ ndis, data = data, span = span)),
      Q95_smooth = predict(loess(Q95 ~ ndis, data = data, span = span))
    )
  return(data_smooth)
}
# Function to smooth values for a single group
smooth_values_group <- function(df, span = 0.9) {
  df %>%
    mutate(
      Mean_smooth = predict(loess(Mean ~ tdis, data = df, span = span)),
      Q05_smooth = predict(loess(Q05 ~ tdis, data = df, span = span)),
      Q95_smooth = predict(loess(Q95 ~ tdis, data = df, span = span))
    )
}


# Apply smoothing function
global_smooth <- smooth_values(global, "ndis", c("Mean", "Q05", "Q95"))
global_smooth%<>%left_join(dis_nt,by="ndis",relationship="many-to-one")

# Fit a LOESS model to map ndis to tdis
ndis_to_tdis_model <- loess(ndis ~ tdis, data = global_smooth, span = 0.9)
# Define a transformation function
ndis_to_tdis_transform <- function(tdis_val) {
  predict(ndis_to_tdis_model, newdata = data.frame(tdis = tdis_val))
}
# Plot it with two axis: total no disasters and number of disasters per country
p <- ggplot(global_smooth, aes(x = tdis)) +
  geom_ribbon(aes(ymin = Q05_smooth, ymax = Q95_smooth, fill = "90% CI"), alpha = 0.2) +  # Smoothed Ribbon
  geom_line(aes(y = Mean_smooth, colour = "Mean"), size = 1) +  # Smoothed Mean
  geom_line(aes(y = Q05_smooth, colour = "Q05"), linetype = "dashed") +  # Smoothed Q05
  geom_line(aes(y = Q95_smooth, colour = "Q95"), linetype = "dashed") +  # Smoothed Q95
  scale_colour_manual(values = c("Mean" = "black", "Q05" = "blue", "Q95" = "blue")) + 
  scale_fill_manual(values = c("90% CI" = "blue")) + 
  scale_x_continuous(
    name = "Total Disasters Included",  # Lower x-axis label (tdis)
    sec.axis = sec_axis(~ ndis_to_tdis_transform(.), 
                        name = "Number of Disasters per Country",
                        breaks=c(3,6,9,12),
                        labels=c(3,6,9,12))  # Upper x-axis (ndis)
  ) +
  labs(y = "Losses [Trillion USD-2017]",
       title = "Agricultural & Food Losses by No. Disasters Included",
       colour = "Metrics",
       fill = "Confidence Interval") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1),  # Rotate x labels
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = "right",
        axis.title.x.top = element_text(margin = margin(b = 10)),  # Space for upper axis
        axis.title.x.bottom = element_text(margin = margin(t = 10)));p
ggsave("./Plots/TotalLosses_NoDis.png",p,height=7,width=10)

# Now the same but including per commodity group
# Apply smoothing function to each Item_Group separately
itemres_smooth <- itemres %>%
  left_join(dis_nt,by="ndis",relationship="many-to-one")%>%
  group_by(Item_Group, tdis, ndis) %>%  # Ensure summation by region
  summarise(
    Mean = sum(Mean, na.rm = TRUE),
    Q05 = sum(Q05, na.rm = TRUE),
    Q95 = sum(Q95, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(ndis) & !is.na(tdis) & !is.na(Mean) & !is.na(Q05) & !is.na(Q95)) %>%
  group_split(Item_Group) %>%  # Split data by Item_Group
  map_dfr(smooth_values_group)
# Plot with multiple commodity lines
p <- ggplot(itemres_smooth, aes(x = tdis, colour = Item_Group)) +
  # geom_ribbon(aes(ymin = Q05_smooth, ymax = Q95_smooth, fill = "90% CI"), alpha = 0.2, inherit.aes = FALSE) +  
  geom_line(aes(y = Mean_smooth, group = Item_Group), size = 1) +  
  scale_colour_manual(values = c("red", "blue", "green", "purple", "orange", "brown", "pink")) +  
  scale_fill_manual(values = c("90% CI" = "blue")) + 
  scale_x_continuous(
    name = "Total Disasters Included",  
    sec.axis = sec_axis(~ ndis_to_tdis_transform(.), 
                        name = "Number of Disasters per Country",
                        breaks = c(3,6,9,12),
                        labels = c(3,6,9,12))
  ) +
  labs(y = "Losses [Trillion USD-2017]",
       title = "Agricultural & Food Losses by No. Disasters Included",
       colour = "Commodity Type",
       fill = "Confidence Interval") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1),  
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = "right",
        axis.title.x.top = element_text(margin = margin(b = 10)),  
        axis.title.x.bottom = element_text(margin = margin(t = 10)));p
ggsave("./Plots/CommodityLosses_NoDis.png",p,height=7,width=10)

# Now for regions
# Apply smoothing function to each Item_Group separately
isores_smooth <- isores %>%
  left_join(dis_nt, by = "ndis", relationship = "many-to-one") %>%
  mutate(UN.Region = ifelse(is.na(UN.Region), "Not Classified", UN.Region)) %>%  # Assign NA to "Not Classified"
  group_by(UN.Region, tdis, ndis) %>%  # Ensure summation by region
  reframe(
    Mean = sum(Mean, na.rm = TRUE),
    Q05 = sum(Q05, na.rm = TRUE),
    Q95 = sum(Q95, na.rm = TRUE)
  ) %>%
  filter(!is.na(ndis) & !is.na(tdis) & !is.na(Mean) & !is.na(Q05) & !is.na(Q95)) %>%
  group_split(UN.Region) %>%  # Split data by UN.Region
  map_dfr(smooth_values_group)
# Plot with multiple commodity lines
p <- ggplot(isores_smooth, aes(x = tdis, colour = UN.Region)) +
  # geom_ribbon(aes(ymin = Q05_smooth, ymax = Q95_smooth, fill = "90% CI"), alpha = 0.2, inherit.aes = FALSE) +  
  geom_line(aes(y = Mean_smooth, group = UN.Region), size = 1) +  
  scale_colour_manual(values = c("red", "blue", "green", "purple", "orange", "brown", "pink")) +  
  scale_fill_manual(values = c("90% CI" = "blue")) + 
  scale_x_continuous(
    name = "Total Disasters Included",  
    sec.axis = sec_axis(~ ndis_to_tdis_transform(.), 
                        name = "Number of Disasters per Country",
                        breaks = c(3,6,9,12),
                        labels = c(3,6,9,12))
  ) +
  labs(y = "Losses [Trillion USD-2017]",
       title = "Agricultural & Food Losses by No. Disasters Included",
       colour = "Region",
       fill = "Confidence Interval") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1),  
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = "right",
        axis.title.x.top = element_text(margin = margin(b = 10)),  
        axis.title.x.bottom = element_text(margin = margin(t = 10)));p
ggsave("./Plots/RegionLosses_NoDis.png",p,height=7,width=10)



