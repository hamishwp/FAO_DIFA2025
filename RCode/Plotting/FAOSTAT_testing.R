ar1_local_loglik_test <- function(y, window = 5) {
  n <- length(y)
  if (sum(y) <= 0) return(data.frame(loglik = rep(NA, length(y) - 1)))
  
  sigma_hat<-max(sqrt(Rfast::ar1(y[!is.na(y)])["sigma"]),0.4)
  
  # Rolling log-likelihood using locally estimated AR(1)
  logliks <- sapply((window + 1):n, function(k) {
    y_window <- y[(k - window):(k)]
    
    y_lag <- y_window[-length(y_window)]
    y_now <- y_window[-1]
    
    # Estimate phi
    phi_hat <- sum(y_lag * y_now) / sum(y_lag^2)
    
    # Residuals and estimated sigma
    residuals <- y_now - phi_hat * y_lag
    # sigma_hat <- max(sqrt(mean(residuals^2)), 1e-6)
    
    # Compute log-likelihood under local phi and sigma
    sum(stats::dnorm(residuals, mean = 0, sd = sigma_hat, log = TRUE))
  })
  
  return(data.frame(
    start_index = (window + 1):n,
    loglik = logliks
  ))
}


difa<-readRDS("./Data/Results/difa2025.RData") 
y<-difa$faostat$Prod
ncoms<-length(unique(y$Item.Code))
items<-unique(y$Item.Code)
itemlab<-unique(y$Item)
tester <- dplyr::bind_rows(parallel::mclapply(1:length(unique(y$ISO3.CODE)), function(i){
  iso<-unique(y$ISO3.CODE)[i]
  print(i)
  yy<-y[y$ISO3.CODE==iso,]
  out<-as.data.frame.array(t(vapply(1:ncoms, function(k){
    yyy<-yy$Production[yy$Item.Code==items[k]]
    yyy<-yyy[!is.na(yyy)]
    if(length(yyy[yyy>0])<=5) return(c(rep(NA_real_,3),mean(yyy,na.rm = T)))
    if(sum(yyy,na.rm = T)<=0) return(c(rep(NA_real_,3),mean(yyy,na.rm = T)))
    if(all(diff(yyy)==0)) return(c(rep(0,3),mean(yyy,na.rm = T)))
    tmp<-tryCatch(ar1_local_loglik_test(yyy)$loglik, error = function(e) NA)
    if(all(is.na(tmp))) {
      warning(paste0("i and k = ",i,", ",k))
      return(c(rep(99,3),mean(yyy,na.rm = T)))
    }
    unname(c(min(tmp,na.rm = T),quantile(tmp,c(0.05),na.rm = T),median(tmp,na.rm = T),mean(yyy,na.rm = T)))
  },numeric(4))))
  # Add items
  out$item<-itemlab; out$ISO3C<-iso
  # Add column names
  names(out)<-c("minLL","q05LL","medLL","meanVal","item","ISO3C")
  return(out)
}, mc.cores = ncores))

tester%>%filter(minLL==99)%>%pull(ISO3C)
tester%>%filter(minLL==0)%>%pull(ISO3C)%>%table()
tester%>%filter(minLL>=0)%>%dplyr::select(ISO3C,item,meanVal,minLL)

tester%>%filter(minLL<99)%>%
  ggplot(aes(meanVal,minLL))+geom_point(aes(colour=ISO3C))+scale_x_log10()

tester%>%filter(minLL<99 & item=="Wheat")%>%
  ggplot(aes(meanVal,minLL))+geom_point(aes(colour=ISO3C))+scale_x_log10()

tester%>%filter(minLL<99 & meanVal>10)%>%
  ggplot(aes(meanVal,minLL/log(meanVal)))+geom_point(aes(colour=ISO3C))+scale_x_log10()

hist(log10(tester$meanVal))
tester%>%group_by(ISO3C)%>%
  reframe(min=min())

# Step 1: Fit quantile regression (e.g., 95th percentile)
fit_qr <- quantreg::rq(minLL ~ log(meanVal), data = tester, tau = 0.995)

# Step 2: Predict 95% boundary
tester$pred95 <- predict(fit_qr, newdata = tester)

# Step 3: Flag anomalies above the 95% boundary
tester$is_anomalous <- tester$minLL > tester$pred95
tester$pvally <- pmax(0,tester$minLL-tester$pred95)

tester%>%filter(is_anomalous)%>%pull(ISO3C)%>%table()

tester%>%filter(is_anomalous)%>%arrange(desc(pvally))%>%dplyr::select(ISO3C,item,meanVal,minLL,pvally)%>%View()

# Step 4: Plot it
plot(log(tester$meanVal), tester$minLL, pch = 16, cex = 0.7,
     col = ifelse(tester$is_anomalous, "red", "black"),
     main = "minLL vs log(meanVal) with 95% quantile boundary",
     xlab = "log(mean production)", ylab = "min local log-likelihood")


# Overlay quantile regression line
curve(predict(fit_qr, newdata = data.frame(meanVal = exp(x))), add = TRUE, col = "blue", lwd = 2)













run_growth_anomaly_test_fast <- function(y, window = 5, n_sim = 1000) {
  if (!is.numeric(y)) stop("Input y must be numeric.")
  if (length(y) < window + 1) stop("Time series too short for given window length.")
  
  n <- length(y)
  window_indices <- 1:(n - window)
  
  # Step 1: Observed statistics
  stat_obs <- vapply(window_indices, function(k) {
    armean <- mean(diff(y[k:(k + window)]))
    sum((y[(k + 1):(k + window)] - armean * y[k:(k + window - 1)])^2)
  }, numeric(1))
  
  # Step 2: Fit global AR(1)
  arfit <- stats::arima(y, order = c(1, 0, 0))
  phi <- stats::coef(arfit)["ar1"]
  sigma <- sqrt(arfit$sigma2)
  
  # Step 3: Simulate and compute stats
  stat_sim <- matrix(NA_real_, nrow = n_sim, ncol = length(window_indices))
  
  for (s in seq_len(n_sim)) {
    ysim <- numeric(n)
    ysim[1] <- y[1]
    innovations <- stats::rnorm(n - 1, mean = 0, sd = sigma)
    ysim[2:n] <- phi * ysim[1:(n - 1)] + innovations
    
    stat_sim[s, ] <- vapply(window_indices, function(k) {
      armean_sim <- mean(diff(ysim[k:(k + window)]))
      sum((ysim[(k + 1):(k + window)] - armean_sim * ysim[k:(k + window - 1)])^2)
    }, numeric(1))
  }
  
  # Step 4: Empirical p-values
  pvals <- vapply(seq_along(stat_obs), function(i) {
    sum(stat_sim[, i] <= stat_obs[i])
  }, numeric(1))
  
  # Step 5: Output
  data.frame(
    minObs = min(stat_obs),
    medObs = median(stat_obs),
    q05Obs = quantile(stat_obs,c(0.05)),
    
    
    
    
    
    pval = pvals
  )
}























# Filter for official data
official_data <- Production %>%
  # filter(FLAG == "A") %>%
  group_by(ISO3.CODE, Year) %>%
  summarise(total_prod = sum(Production, na.rm = TRUE), .groups = "drop")

# Sort and calculate diff and percentage diff
official_diff <- official_data %>%
  arrange(ISO3.CODE, Year) %>%
  group_by(ISO3.CODE) %>%
  mutate(
    diff = total_prod - lag(total_prod),
    pct_diff = (total_prod - lag(total_prod)) / lag(total_prod) * 100,
    median_prod = median(total_prod, na.rm = TRUE)
  ) %>%
  ungroup()

ggplot(official_diff, aes(x = median_prod, y = diff, colour = ISO3.CODE)) +
  geom_point() +
  theme_minimal() + scale_y_log10() + scale_x_log10() +
  labs(title = "Year-on-Year Absolute Production Change vs. Median (Official Data)",
       x = "Median Production (Tonnes)",
       y = "Difference in Production (Tonnes)")

ggplot(official_diff, aes(x = median_prod, y = pct_diff, colour = ISO3.CODE)) +
  geom_point() +
  theme_minimal() + scale_x_log10() + scale_y_log10() +
  labs(title = "Year-on-Year % Production Change vs. Median (Official Data)",
       x = "Median Production (Tonnes)",
       y = "Percentage Change (%)")

hist(log10(abs(official_diff$pct_diff[!is.infinite(official_diff$pct_diff) & !is.na(official_diff$pct_diff)])))

rolling_stats <- official_data %>%
  arrange(ISO3.CODE, Year) %>%
  group_by(ISO3.CODE) %>%
  mutate(
    roll_diff = zoo::rollapply(total_prod, 5, function(x) max(x) - min(x), fill = NA, align = "right"),
    roll_pct_diff = zoo::rollapply(total_prod, 5, function(x) (max(x) - min(x)) / median(x) * 100, fill = NA, align = "right"),
    roll_median = zoo::rollapply(total_prod, 5, median, fill = NA, align = "right")
  ) %>%
  filter(!is.na(roll_diff)) %>%
  ungroup()


ggplot(rolling_stats, aes(x = roll_median, y = roll_diff, colour = ISO3.CODE)) +
  geom_point() +
  theme_minimal() + scale_x_log10() + scale_y_log10() + 
  labs(title = "5-Year Rolling Absolute Change vs. Median (Official Data)",
       x = "Rolling 5-Year Median Production",
       y = "Rolling Absolute Difference (Tonnes)")


ggplot(rolling_stats, aes(x = roll_median, y = roll_pct_diff, colour = ISO3.CODE)) +
  geom_point() +
  theme_minimal() +scale_x_log10() + scale_y_log10() + 
  labs(title = "5-Year Rolling % Change vs. Median (Official Data)",
       x = "Rolling 5-Year Median Production",
       y = "Rolling Percentage Change (%)")






df_sorted <- Production %>%
  select(ISO3.CODE, Item, Year, Production, FLAG) %>%
  arrange(ISO3.CODE, Item, Year)

compare_to_last_official <- df_sorted %>%
  group_by(ISO3.CODE, Item) %>%
  mutate(
    # Store most recent official value up to each year
    last_official = zoo::na.locf(ifelse(FLAG == "A", Production, NA), na.rm = FALSE),
    last_official_year = zoo::na.locf(ifelse(FLAG == "A", Year, NA), na.rm = FALSE)
  ) %>%
  filter(FLAG %in% c("E", "I") & !is.na(last_official)) %>%
  mutate(
    abs_diff = Production - last_official,
    pct_diff = (Production - last_official) / last_official * 100
  ) %>%
  ungroup()

ggplot(compare_to_last_official, aes(x = ISO3.CODE, y = abs_diff, fill = FLAG)) +
  geom_boxplot() +
  theme_minimal() + scale_y_log10() + 
  labs(title = "Absolute Difference from Last Official Estimate",
       x = "Country",
       y = "Absolute Difference (Tonnes)")

ggplot(compare_to_last_official, aes(x = ISO3.CODE, y = pct_diff, fill = FLAG)) +
  geom_boxplot() +
  theme_minimal() + scale_y_log10() + 
  labs(title = "% Difference from Last Official Estimate",
       x = "Country",
       y = "Percentage Difference (%)")

ggplot(compare_to_last_official, aes(x = abs_diff, y = pct_diff)) +
  geom_point(aes(colour=ISO3.CODE)) +
  scale_y_log10() + scale_x_log10() +
  labs(title = "Difference from Last Official Estimate",
       x = "Absolute Difference (Tonnes)",
       y = "Percentage Difference (%)")+
  facet_wrap(~FLAG)


summary_table <- compare_to_last_official %>%
  mutate(Year=as.integer(Year),
         last_official_year=as.integer(last_official_year),
         abs_diff = ifelse(is.infinite(abs_diff), NA, abs_diff),
         pct_diff = ifelse(is.infinite(pct_diff), NA, pct_diff))%>%
  group_by(ISO3.CODE, FLAG) %>%
  summarise(
    n = n(),
    median_abs_diff = median(abs_diff, na.rm = TRUE),
    mean_abs_diff = mean(abs_diff, na.rm = TRUE),
    median_pct_diff = median(pct_diff, na.rm = TRUE),
    mean_pct_diff = mean(pct_diff, na.rm = TRUE),
    med_gap_years = median(Year - last_official_year, na.rm = TRUE),
    max_gap_years = max(Year - last_official_year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(abs(median_pct_diff)))

ggplot(summary_table, aes(x = abs(median_abs_diff), y = abs(median_pct_diff))) +
  geom_point(aes(colour=ISO3.CODE)) +
  scale_y_log10() + scale_x_log10() +
  labs(title = "Difference from Last Official Estimate",
       x = "Absolute Difference (Tonnes)",
       y = "Percentage Difference (%)")+
  facet_wrap(~FLAG)

summary_by_item <- compare_to_last_official %>%
  group_by(ISO3.CODE, Item, FLAG) %>%
  summarise(
    n = n(),
    median_pct_diff = median(pct_diff, na.rm = TRUE),
    median_abs_diff = median(abs_diff, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n >= 3) %>%  # filter for reliability
  arrange(desc(abs(median_pct_diff)))

ggplot(summary_by_item, aes(x = abs(median_abs_diff), y = abs(median_pct_diff))) +
  geom_point(aes(colour=ISO3.CODE)) +
  scale_y_log10() + scale_x_log10() +
  labs(title = "Difference from Last Official Estimate",
       x = "Absolute Difference (Tonnes)",
       y = "Percentage Difference (%)")+
  facet_wrap(~FLAG)

View(summary_table)
















mcmc_results$mGPR%>%filter(posterior < quantile(mcmc_results$mGPR$posterior, 0.75))%>%ggplot()+geom_point(aes(beta_dis,abs(posterior)))+
  geom_vline(xintercept = 0,colour="black")+scale_y_log10()+
  xlab("Beta - Disaster Severity")+ylab("Likelihood (Log)")

des_results$mGPR%>%filter(posterior > quantile(des_results$mGPR$posterior, 0.75))%>%ggplot()+geom_point(aes(beta_dis,abs(posterior)))+
  geom_vline(xintercept = 0,colour="black")+scale_y_log10()+
  xlab("Beta - Disaster Severity")+ylab("Likelihood (Log)")

out%>%filter(posterior > quantile(out$posterior, 0.75))%>%ggplot()+geom_point(aes(beta_dis,abs(posterior)))+
  geom_vline(xintercept = 0,colour="black")+scale_y_log10()+
  xlab("Beta - Disaster Severity")+ylab("Likelihood (Log)")

mGPR%>%filter(posterior > quantile(mGPR$posterior, 0.75))%>%ggplot()+geom_point(aes(beta_dis,abs(posterior)))+
  geom_vline(xintercept = 0,colour="black")+scale_y_log10()+
  xlab("Beta - Disaster Severity")+ylab("Likelihood (Log)")


mcmc_results$mGPR%>%ggplot(aes(log10(abs(posterior-max(posterior)))))+
                             geom_density(aes(fill=beta_dis<0)) + scale_y_log10()










y%>%group_by(ISO3.CODE, Year)%>%
  reframe(perc=sum(FLAG%in%c("A","")/sum(!FLAG%in%c("X","M"))),
          comb=paste0(sort(Item.Code),collapse = "-"))%>%
  group_by(ISO3.CODE)%>%
  reframe(avperc=mean(perc),
          sdperc=sd(perc))%>%View()














