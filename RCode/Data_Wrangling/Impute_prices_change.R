library(abind)
library(dplyr)
library(magrittr)
library(tidyverse)
library(readxl)
library(ggplot2)
library(countrycode)
library(stringr)
library(pracma)
library(data.table)
library(FAOSTAT)

DATA <- readRDS("C:/Users/ignac/Desktop/FAO_DIFA2025/Data/Results/difa2025.RData")
faostat <- DATA$faostat

ImputePrices <- function(price_df, missthresh_GPR = 0.3, missthresh_nearest = 0.6) {
  all_items <- unique(price_df$item_grouping_f)
  all_years <- seq(min(price_df$Year), max(price_df$Year))
  total_years <- length(all_years)
  all_isos <- unique(price_df$ISO3.CODE)
  
  mu_price <- array(NA, dim = c(length(all_isos), length(all_years), length(all_items)), 
                    dimnames = list(all_isos, all_years, all_items))
  sig_price <- array(NA, dim = c(length(all_isos), length(all_years), length(all_items)), 
                     dimnames = list(all_isos, all_years, all_items))
  
  for (i in 1:length(all_isos)) {
    iso <- all_isos[i]
    for (j in 1:length(all_items)) {
      item <- all_items[j]
      
      item_data <- price_df %>% 
        filter(ISO3.CODE == iso & item_grouping_f == item) %>% 
        arrange(Year)
      
      if (nrow(item_data) == 0) {
        mu_price[i, , j] <- 0
        sig_price[i, , j] <- 1e-6
        next
      }
      
      indies <- is.na(item_data$Price17eq)
      missing_years <- all_years[indies]
      missing_proportion <- length(missing_years) / total_years
      prediction_years <- item_data$Year[indies]
      observed_years <- item_data$Year[!indies]
      observed_prices <- item_data$Price17eq[!indies]
      
      mu_price[i, , j] <- item_data$Price17eq
      sig_price[i, , j] <- 1e-6
      
      if (missing_proportion > missthresh_nearest) {
        if (length(observed_prices[!is.na(observed_prices)]) == 0) {
          mu_price[i, indies, j] <- NA
          sig_price[i, indies, j] <- 1e-6
        } else {
          nearest_values <- approx(
            x = observed_years, 
            y = observed_prices, 
            xout = prediction_years, 
            method = "constant", 
            rule = 2
          )
          mu_price[i, indies, j] <- nearest_values$y
          global_sd <- ifelse(length(observed_prices) > 1, sd(observed_prices, na.rm = TRUE), 1e-6)
          sig_price[i, indies, j] <- global_sd + 1e-6
        }
      } else if (missing_proportion > missthresh_GPR) {
        cv_results <- crossval_prices(observed_years, observed_prices, prediction_years, method = "GPR")
        mu_price[i, indies, j] <- cv_results$mu_price
        sig_price[i, indies, j] <- cv_results$sig_price
      } else {
        cv_results <- crossval_prices(observed_years, observed_prices, prediction_years, method = "spline")
        mu_price[i, indies, j] <- cv_results$mu_price
        sig_price[i, indies, j] <- cv_results$sig_price
      }
    }
  }
  
  return(list(mu_price = mu_price, sig_price = sig_price))
}


prices <- faostat$price %>%
  filter(Year <= 2023)%>%
  left_join(faostat$item_groups, by = c("Item")) %>%
  filter(!is.na(item_grouping_f)) %>%
  left_join(
    faostat$Prod %>%
      filter(Production > 1) %>%
      transmute(
        ISO3.CODE,
        Item,
        Year = as.numeric(Year),
        Log_Prod = log(Production)
      ),
    by = c("ISO3.CODE", "Item", "Year")
  ) %>%
  mutate(
    Price_aux = fifelse(is.na(Log_Prod),NA,Price17eq)
  )%>%
  group_by(ISO3.CODE, item_grouping_f, Year) %>%
  reframe(
    Price17eq = weighted.mean(
      Price_aux,  
      Log_Prod,  
      na.rm = TRUE
    )
  ) %>%
  mutate(
    Price17eq = fifelse(is.nan(Price17eq),NA,Price17eq)
  )%>%
  ImputePrices()

Complete_Prices <- function(Prices, Prod, FAOSTAT_Item) {
  
  mu_price <- Prices$mu_price
  sig_price <- Prices$sig_price
  
  all_items <- unique(FAOSTAT_Item$item_grouping_f)
  all_years <- seq(min(Prod$Year), max(Prod$Year))
  all_isos <- unique(Prod$ISO3.CODE)
  
  for (iso in all_isos) {
    
    if (!(iso %in% rownames(mu_price))) {
      mu_price <- abind(mu_price, array(NA, dim = c(1, length(all_years), length(all_items))), along = 1)
      rownames(mu_price)[nrow(mu_price)] <- iso
      sig_price <- abind(sig_price, array(NA, dim = c(1, length(all_years), length(all_items))), along = 1)
      rownames(sig_price)[nrow(sig_price)] <- iso
      dimnames(mu_price)[[3]] <- all_items
      dimnames(sig_price)[[3]] <- all_items
    }
    
    current_items <- dimnames(mu_price[iso, , , drop = FALSE])[[3]]
    missing_items <- setdiff(all_items, current_items)
    
    if (length(missing_items) > 0) {
      iso_mu <- mu_price[iso, , , drop = FALSE]
      iso_sig <- sig_price[iso, , , drop = FALSE]
      
      new_mu <- array(
        NA, 
        dim = c(1, length(all_years), length(missing_items)),
         dimnames = list(iso, all_years, missing_items))
      
      new_sig <- array(
        NA,
        dim = c(1, length(all_years), length(missing_items)),
        dimnames = list(iso, all_years, missing_items))
      
      iso_mu <- abind(iso_mu, new_mu, along = 3)
      iso_sig <- abind(iso_sig, new_sig, along = 3)
      
      order_index <- match(all_items, dimnames(iso_mu)[[3]])
      
      iso_mu <- iso_mu[, , order_index, drop = FALSE]
      iso_sig <- iso_sig[, , order_index, drop = FALSE]
      
      mu_price[iso, , ] <- iso_mu[1, , ]
      sig_price[iso, , ] <- iso_sig[1, , ]
      
      dimnames(mu_price)[[3]] <- all_items
      dimnames(sig_price)[[3]] <- all_items
    }
  }
  
  return(list(mu_price = mu_price, sig_price = sig_price))
}


prices2 <- prices%>%
  Complete_Prices(
    Prod = faostat$Prod%>%filter(Year >= 1991), 
    FAOSTAT_Item = faostat$item_groups
    )

country_region = read.csv("Data/RawData/country_region.csv")%>% 
  select(
    - X
  )

ImputeRegion <- function(prices, country_region) {
  
  mu_price <- prices$mu_price
  sig_price <- prices$sig_price
  
  isos <- dimnames(mu_price)[[1]]
  years <- dimnames(mu_price)[[2]]
  items <- dimnames(mu_price)[[3]]
  
  iso2sub <- setNames(as.character(country_region$unsd_sub_reg), country_region$ISO)
  iso2macro <- setNames(as.character(country_region$unsd_macro_reg), country_region$ISO)
  
  for (j in seq_along(years)) {
    for (k in seq_along(items)) {
      prices_vec <- mu_price[, j, k]
      global_median <- if (all(is.na(prices_vec))) NA else median(prices_vec, na.rm = TRUE)
      
      sub_regions <- unique(iso2sub[isos])
      sub_regions <- sub_regions[!is.na(sub_regions)]
      subregion_medians <- sapply(sub_regions, function(subr) {
        indices <- which(iso2sub[isos] == subr)
        vec <- prices_vec[indices]
        if (all(is.na(vec))) NA else median(vec, na.rm = TRUE)
      })
      
      macro_regions <- unique(iso2macro[isos])
      macro_regions <- macro_regions[!is.na(macro_regions)]
      macro_medians <- sapply(macro_regions, function(macro) {
        indices <- which(iso2macro[isos] == macro)
        vec <- prices_vec[indices]
        if (all(is.na(vec))) NA else median(vec, na.rm = TRUE)
      })
      
      for (i in seq_along(isos)) {
        if (is.na(mu_price[i, j, k])) {
          subr <- iso2sub[isos[i]]
          macro <- iso2macro[isos[i]]
          imputed_val <- if (!is.na(subr) && !is.na(subregion_medians[subr])) {
            subregion_medians[subr]
          } else if (!is.na(macro) && !is.na(macro_medians[macro])) {
            macro_medians[macro]
          } else {
            global_median
          }
          mu_price[i, j, k] <- imputed_val
        }
      }
    }
  }
  
  
  return(list(mu_price = mu_price, sig_price = sig_price))
}

prices3 <- prices2%>%
  ImputeRegion(
    country_region
  )

save(prices3,file = "Prices.RData")
