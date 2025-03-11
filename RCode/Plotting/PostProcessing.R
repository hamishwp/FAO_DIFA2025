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


# Assuming you have your `results_df` and `fdf` (with n_com and n_haz defined):
params <- convert_results_to_params(results_df, fdf)






prices<-readRDS("./Data/Results/Prices 1.RData")

# First, extract the rownames (country ISO3C codes) from the arrays
iso_names_prices <- rownames(prices$mu_price)  # assuming rownames are correctly assigned

# Match fdf$isos to the ISO3C codes of the arrays to get the correct index order
reorder_idx <- match(fdf$isos, iso_names_prices)

# Check for unmatched countries (optional safety check)
if (any(is.na(reorder_idx))) {
  warning("Some countries in fdf$isos were not found in prices3$mu_price rownames.")
}

# Reorder the arrays along the first dimension (countries)
prices3$mu_price <- prices3$mu_price[reorder_idx, , , drop = FALSE]
prices3$sig_price <- prices3$sig_price[reorder_idx, , , drop = FALSE]

# (Optional) double-check the rownames now match
if (!all(rownames(prices3$mu_price) == fdf$isos)) {
  warning("Mismatch after reordering. Please verify country codes.")
}

# Done. Now prices3$mu_price and prices3$sig_price are ordered as fdf$isos


fdf$mu_prices<-prices3$mu_price
fdf$sig_prices<-prices3$sig_price

params <- convert_results_to_params(results_df, fdf, "w_mean")
losses<-predloss_bin_par(fdf, params, mxdis=5, n_sam=5000)
params$beta_dis <- convert_results_to_params(results_df, fdf, "w_q75")$beta_dis
tmp<-predloss_bin_par(fdf, params, mxdis=5, n_sam=5000)
losses$losses <- abind::abind(losses$losses, tmp$losses, along = 4)
losses$all_losses <- abind::abind(losses$all_losses, tmp$all_losses, along = 2)
params$beta_dis <- convert_results_to_params(results_df, fdf, "w_q25")$beta_dis
tmp<-predloss_bin_par(fdf, params, mxdis=5, n_sam=5000)
losses$losses <- abind::abind(losses$losses, tmp$losses, along = 4)
losses$all_losses <- abind::abind(losses$all_losses, tmp$all_losses, along = 2)

print(paste0("Mean: ",sum(apply(losses$all_losses,1,mean))/1e12))
print(paste0("Q05: ",(sum(apply(losses$all_losses,1,quantile,probs=c(0.05)))/1e12)))
print(paste0("Q95: ",(sum(apply(losses$all_losses,1,quantile,probs=c(0.95)))/1e12)))

itemres<-data.frame(Item_Group=fdf$commods,
           Mean=apply(losses$all_losses,1,mean)/1e12,
           Q05=apply(losses$all_losses,1,quantile,probs=c(0.05))/1e12,
           Q95=apply(losses$all_losses,1,quantile,probs=c(0.95))/1e12)

# Barplot with error bars
p<-ggplot(itemres, aes(x = Item_Group, y = Mean, fill = Item_Group)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +  # Bar plot
  geom_errorbar(aes(ymin = Q05, ymax = Q95), width = 0.2) +   # Error bars
  # theme_minimal() +  # Clean theme
  labs(y = "Losses [Trillion USD-2017]",
       x = "Commodity Group",
       title = "Estimated Agricultural & Food Losses by Commodity",
       fill = "Item Group") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x labels
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5));p
ggsave("./Plots/TotalCommodityLosses.png",p,height=6,width=8)

isores<-data.frame(ISO3=fdf$isos,
                    Mean=sapply(1:fdf$n_isos,function(i) sum(sapply(1:fdf$n_t,function(j) sum(sapply(1:fdf$n_com,function(k) mean(losses$losses[i,j,k,]))))))/1e12,
                    Q05=sapply(1:fdf$n_isos,function(i) sum(sapply(1:fdf$n_t,function(j) sum(sapply(1:fdf$n_com,function(k) quantile(losses$losses[i,j,k,],probs = c(0.05)))))))/1e12,
                    Q95=sapply(1:fdf$n_isos,function(i) sum(sapply(1:fdf$n_t,function(j) sum(sapply(1:fdf$n_com,function(k) quantile(losses$losses[i,j,k,],probs = c(0.95)))))))/1e12)

country_region = openxlsx::read.xlsx("Data/Taxonomies/IsoContinentRegion.xlsx")%>% 
  dplyr::select(ISO.Code, Country, UN.Region, UN.Sub.Region, World.Bank.Income.Groups)%>%rename(ISO3=ISO.Code)
isores%<>%left_join(country_region,by="ISO3",relationship="one-to-one")

p<-isores%>%group_by(UN.Region)%>%reframe(Mean=sum(Mean),Q05=sum(Q05),Q95=sum(Q95))%>%
  filter(!is.na(UN.Region) & UN.Region!="Not Classified")%>%
  ggplot(aes(x = UN.Region, y = Mean, fill = UN.Region)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +  # Bar plot
  geom_errorbar(aes(ymin = Q05, ymax = Q95), width = 0.2) +   # Error bars
  # theme_minimal() +  # Clean theme
  labs(y = "Losses [Trillion USD-2017]",
       x = "Region",
       title = "Estimated Agricultural & Food Losses by Region",
       fill = "Region") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x labels
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5));p
ggsave("./Plots/TotalRegionLosses.png",p,height=6,width=8)

p<-isores%>%mutate(World.Bank.Income.Groups=case_when(is.na(World.Bank.Income.Groups)~"SIDS",T~World.Bank.Income.Groups))%>%
  group_by(World.Bank.Income.Groups)%>%reframe(Mean=sum(Mean),Q05=sum(Q05),Q95=sum(Q95))%>%
  filter(!is.na(World.Bank.Income.Groups) & World.Bank.Income.Groups!="Not Classified")%>%
  ggplot(aes(x = World.Bank.Income.Groups, y = Mean, fill = World.Bank.Income.Groups)) +
  geom_bar(stat = "identity", width = 0.7, color = "black") +  # Bar plot
  geom_errorbar(aes(ymin = Q05, ymax = Q95), width = 0.2) +   # Error bars
  # theme_minimal() +  # Clean theme
  labs(y = "Losses [Trillion USD-2017]",
       x = "Income Group",
       title = "Estimated Agricultural & Food Losses by Income Group",
       fill = "Income Group") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x labels
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5));p
ggsave("./Plots/TotalIncomeGroupLosses.png",p,height=6,width=8)



