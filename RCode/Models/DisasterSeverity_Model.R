
if(Desinventar){ # infer disaster severity from Desinventar data
  # Extract the disaster severity estimates
  GetDisSev<-function(dissie,emdat){
    # We need a model for the following features:
    #   input: 
    #       - Desinventar - crops, cattle, deaths, cost, affected, haz_Ab, year, duration, region, income group
    #       - World Bank - population, GDP, surface area, year
    #       - EM-DAT - deaths, cost, affected, haz_Ab, year, duration, region, income group
    #   output:
    #       - multivariate model to predict crops + cattle
    #       - six models, one for when each impact type combination is present (CHECK BIC VALUES)
    #       - independent variables include deaths, cost, affected and their normalised features
    #       - adjusts for (grouped) hazard type
    #       - adjusts for region (categorical) and income group (ordinal)
    
    #   Questions:
    #       - decide sampling method to minimise bias in the data
    #       - Create mixed effects model with region and haz_Ab as fixed effects and income group as random effect
    #       - Create re-sampling technique to reduce temporal bias in EM-DAT
    #       - Create re-sampling technique to reduce country bias in EM-DAT
    #       - Create re-sampling technique to reduce hazard bias in EM-DAT
    
    # Modify certain variables
    dissie%<>%mutate_at(c("duration","deaths","cost","affected","crops","cattle"), function(x) log(x+10))
    # Set number of folds
    num_folds <- 5
    # Define independent variables
    imp_sets <- list(
      c("deaths", "cost", "affected"),
      c("norm_deaths", "norm_cost", "norm_affected"))
    # Define fixed and random effect variables
    fixed_effects_all <- c("region","haz_grp")
    # Random effect for income group
    random_effects_all <- c("duration","incomegrp")
    # Make income ordinal
    dissie%<>%mutate(incomegrp=case_when(is.na(incomegrp)~"0", 
                                         incomegrp=="Low Income"~"0",
                                         incomegrp=="Middle Income"~"1",
                                         incomegrp=="High Income"~"2", TRUE ~ "0"))%>%mutate(incomegrp=as.integer(incomegrp))
    # Combinations of all possible model formulations
    generate_subsets <- function(vars) {
      unlist(lapply(1:length(vars), function(i) combinat::combn(vars, i, simplify = FALSE)), recursive = FALSE)
    }
    impact_combinations <- generate_subsets(unique(unlist(imp_sets))) # Impact variables subsets
    fixed_combinations <- generate_subsets(fixed_effects_all) # Fixed effects subsets
    random_combinations <- generate_subsets(random_effects_all) # Random effects subsets
    # Define dependent variables
    dep_vars <- c("crops", "cattle") #, "norm_crops", "norm_cattle") #, "cbind(crops,cattle)")
    # Weighting function
    calculate_weights <- function(df, factor = 0) {
      # Compute frequencies for haz_grp, region, and incomegrp
      haz_grp_freq <- df %>% group_by(haz_grp) %>% reframe(freq = n()) %>% mutate(weight_haz = 1 / freq)
      region_freq <- df %>% group_by(region) %>% reframe(freq = n()) %>% mutate(weight_region = 1 / freq)
      incomegrp_freq <- df %>% group_by(incomegrp) %>% reframe(freq = n()) %>% mutate(weight_income = 1 / freq)
      # Merge weights back into the dataset
      df %<>%
        left_join(haz_grp_freq, by = "haz_grp") %>%
        left_join(region_freq, by = "region") %>%
        left_join(incomegrp_freq, by = "incomegrp")
      # Compute final weight: geometric mean of three individual weights
      df %>%
        mutate(base_weight = sqrt(weight_haz * weight_income), 
               weight = factor + (1 - factor) * base_weight) %>%
        mutate(weight=weight/max(weight))%>%
        select(-starts_with("freq"), -starts_with("weight_haz"), -starts_with("weight_region"), -starts_with("weight_income"))
    }
    # How do we want to try to group the hazards?
    hazgrps<-list(
      c("CW"="ET",
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
      "CF"="OT",
      "DZ"="ET"),
    c("CW"="OT",
      "DR"="DR",
      "WF"="WF",
      "FL"="ST",
      "ST"="ST",
      "TC"="ST",
      "EQ"="OT",
      "VO"="OT",
      "AV"="OT",
      "LS"="OT",
      "HW"="OT",
      "TS"="ST",
      "ET"="OT",
      "EP"="ST",
      "VW"="ST",
      "SN"="OT",
      "SS"="ST",
      "MS"="OT",
      "TC:FL"="ST",
      "CF"="OT",
      "DZ"="OT"),
    c("CW"="OT",
      "DR"="DR",
      "WF"="WF",
      "FL"="OT",
      "ST"="OT",
      "TC"="OT",
      "EQ"="OT",
      "VO"="OT",
      "AV"="OT",
      "LS"="OT",
      "HW"="OT",
      "TS"="OT",
      "ET"="OT",
      "EP"="OT",
      "VW"="OT",
      "SN"="OT",
      "SS"="OT",
      "MS"="OT",
      "TC:FL"="OT",
      "CF"="OT",
      "DZ"="OT"),
    c("CW"="OT",
      "DR"="DR",
      "WF"="WF",
      "FL"="FL",
      "ST"="OT",
      "TC"="OT",
      "EQ"="OT",
      "VO"="OT",
      "AV"="OT",
      "LS"="OT",
      "HW"="OT",
      "TS"="OT",
      "ET"="OT",
      "EP"="FL",
      "VW"="OT",
      "SN"="OT",
      "SS"="OT",
      "MS"="OT",
      "TC:FL"="OT",
      "CF"="OT",
      "DZ"="OT"),
    c("CW"="OT",
      "DR"="DR",
      "WF"="WF",
      "FL"="FL",
      "ST"="ST",
      "TC"="ST",
      "EQ"="OT",
      "VO"="OT",
      "AV"="OT",
      "LS"="OT",
      "HW"="OT",
      "TS"="OT",
      "ET"="OT",
      "EP"="FL",
      "VW"="ST",
      "SN"="OT",
      "SS"="OT",
      "MS"="OT",
      "TC:FL"="ST",
      "CF"="OT",
      "DZ"="OT"))
    # Function to create model formula
    F_model <- function(impact_vars, fixed_vars, random_vars, deppie="crops") {
      random_formula <- paste(c("1",impact_vars,random_vars), collapse = " + ")
      fixed_formula <- paste0(paste0("(", random_formula," | "),fixed_vars,")",collapse = " + ")
      formula_str <- paste0(deppie," ~ ", fixed_formula)
      return(as.formula(formula_str))
    }
    # Define weighting factors to iterate over
    weighting_factors <- c(0.); ww<-0.
    # Run it!
    # for(ww in weighting_factors){
    # Dataframe template
    mod_res<-data.frame()
    # Different hazard grouping models
    for (j in 1:length(hazgrps)){
      # Try changing the number of hazard groups
      disloc<-dissie%>%GroupHazs(hazgrps[[j]])
      # For each dependent variable to be modelled
      for(deppie in dep_vars){
        print(deppie)
        # Fit models and store results
        for (impact_vars in impact_combinations) {
          for (fixed_vars in fixed_combinations) {
            for (random_vars in random_combinations) {
              # Ensure only complete records are modelled
              df <- disloc %>% filter(complete.cases(select(., all_of(c(impact_vars, fixed_vars, random_vars)))))%>%
                calculate_weights(factor = ww)
              # Ensure enough data for modeling
              if (nrow(df) > 30) {  
                # Ensure reproducibility
                set.seed(123)
                # Create 10 CV folds
                df$fold <- caret::createFolds(df$weight, k = num_folds, list = FALSE)
                # Model formula
                formula <- F_model(impact_vars, fixed_vars, random_vars, deppie)
                # Store BIC for each fold
                bic_values <- c()
                # Go through folds
                for (kk in 1:num_folds) {
                  # Training and testing split
                  train_data <- df %>% filter(fold != kk)
                  test_data <- df %>% filter(fold == kk)
                  # Run the model
                  model <- suppressMessages(suppressWarnings(try(lme4::lmer(formula = formula, data = train_data, REML = TRUE, verbose = FALSE, weights = train_data$weight), silent = TRUE)))
                  # Store results if successful
                  if (!inherits(model, "try-error")) {
                    # Compute log-likelihood on test data
                    LL_test <- logLik(model, newdata = test_data)
                    # Approximate test BIC using test log-likelihood
                    k <- length(unlist(lme4::ranef(model)))+length(unlist(lme4::fixef(model))) # Number of parameters
                    n_test <- nrow(test_data) # Sample size
                    test_bic <- -2 * as.numeric(LL_test) + k * log(n_test) # BIC value
                    bic_values <- c(bic_values, test_bic)
                  } else {
                    print(paste0("Failed model at fold ", kk, ": ", paste(impact_vars, collapse = ", "), " | ", paste(fixed_vars, collapse = ", "), " | ", paste(random_vars, collapse = ", ")))
                  }
                }
                if(length(bic_values)!=0){
                  # Store average performance across folds
                  mod_res %<>% rbind(data.frame(
                    dep_var = deppie,
                    weight_fac = ww,
                    hazgrp = j,
                    formula = paste0(as.character(formula)[2:3], collapse = " ~ "),
                    BIC = median(bic_values),
                    sBIC = sd(bic_values),
                    n = nrow(df)
                  ))
                }
              }
            }
          }
        }
      }
    }
    # }
    # BIC value per observation
    mod_res%<>%mutate(nBIC=BIC/n)
    # Display the model comparison results
    View(mod_res)
    # Find the ideal hazard grouping
    tby_hzgp<-mod_res%>%filter(weight_fac==0)%>%arrange(BIC)%>%group_by(dep_var,n)%>%slice(1)%>%ungroup()%>%pull(hazgrp)%>%table()
    hazgrp_f<-as.integer(names(tby_hzgp)[which.max(tby_hzgp)])
    # Prepare the Desinventar and EM-DAT datasets
    dissie %<>% GroupHazs(hazgrps[[hazgrp_f]]) %>% calculate_weights(factor = 0)
    emdat %<>% GroupHazs(hazgrps[[hazgrp_f]])
    # Modify certain variables
    emdat%<>%mutate_at(c("duration","deaths","cost","affected"), 
                       function(x) log(x+10))%>%
      mutate(incomegrp=case_when(is.na(incomegrp)~"0", 
                                         incomegrp=="Low Income"~"0",
                                         incomegrp=="Middle Income"~"1",
                                         incomegrp=="High Income"~"2", TRUE ~ "0"))%>%
      mutate(incomegrp=as.integer(incomegrp))
    # Check which impact features have missing values RENAME NORMALISED IMPACTS
    impact_vars <- c("deaths", "cost", "affected", "norm_d", "norm_c", "norm_a")
    # Rename also in the EM-DAT database
    emdat%<>%rename(norm_d=norm_deaths,norm_a=norm_affected,norm_c=norm_cost)
    dissie%<>%rename(norm_d=norm_deaths,norm_a=norm_affected,norm_c=norm_cost)
    # Remove all records where all impacts are NAs
    emdat%<>%filter(!apply(emdat[,impact_vars],1,function(x) all(is.na(x))))
    # Calculate which features have missing values for EM-DAT data so we build a model for each complete-dataset
    missing_patterns <- emdat %>%
      select(all_of(impact_vars)) %>%
      mutate(across(everything(), ~ ifelse(is.na(.), NA_character_, "Present"))) %>%
      distinct()
    # Rename the normalised impacts temporarily, otherwise grepl doesn't work
    mod_res$formula<-str_replace_all(str_replace_all(str_replace_all(mod_res$formula,"norm_deaths","norm_d"),"norm_cost","norm_c"),"norm_affected","norm_a")
    # Create a list to store results
    sevvies <- data.frame()
    # Calculate disaster severity!
    for (i in 1:nrow(missing_patterns)) {
      # Get the current missing pattern
      current_pattern <- missing_patterns[i,]
      # Which models have no missing values for these variables?
      inds <- !logical(nrow(mod_res))
      if(length(impact_vars[is.na(current_pattern)])>0) inds <- inds & !apply(sapply(impact_vars[is.na(current_pattern)], function(x) grepl(x,mod_res$formula)),1,function(y) any(y))
      if(length(impact_vars[!is.na(current_pattern)])>0) inds <- inds & apply(sapply(impact_vars[!is.na(current_pattern)],function(x) grepl(x,mod_res$formula)),1,function(y) any(y))
      # Extract lowest BIC value from these models
      cattle_f<-mod_res[inds,]%>%filter(dep_var=="cattle" & weight_fac==0 & hazgrp==hazgrp_f)%>%
        arrange(nBIC)%>%slice(1)%>%pull(formula)
      crop_f<-mod_res[inds,]%>%filter(dep_var=="crops" & weight_fac==0 & hazgrp==hazgrp_f)%>%
        arrange(nBIC)%>%slice(1)%>%pull(formula)
      # Extract indices from EM-DAT and reduce EM-DAT to avoid double counting
      miniem<-emdat%>%filter(if_all(impact_vars[is.na(current_pattern)],is.na))
      # Train model
      model <- suppressMessages(suppressWarnings(try(lme4::lmer(formula = cattle_f, data = dissie, REML = TRUE, verbose = FALSE, weights = dissie$weight), silent = TRUE)))
      # Predict on EM-DAT
      # Ensure model did not fail
      if (!inherits(model, "try-error")) {
        # Get predicted values
        preds <- predict(model, newdata = miniem, allow.new.levels = TRUE)
        # Extract standard deviation of prediction uncertainty
        uncert <- merTools::predictInterval(model, newdata = miniem, level = 0.95, 
                                            n.sims = 1000, which = "all", include.resid.var = TRUE)
        # The standard deviation is estimated from the posterior distribution
        sddies <- (uncert$upr - uncert$lwr) / (2 * 1.96)
        # Create output dataframe
        sevvies%<>%rbind(cbind(miniem,data.frame(
          mu = as.numeric(preds),
          sd = as.numeric(sddies),
          dep = "cattle")))
      } else {
        stop("Model failed to fit; cannot generate cattle predictions.")
      }
      # Train model
      model <- suppressMessages(suppressWarnings(try(lme4::lmer(formula = crop_f, data = dissie, REML = TRUE, verbose = FALSE, weights = dissie$weight), silent = TRUE)))
      # Predict on EM-DAT
      # Ensure model did not fail
      if (!inherits(model, "try-error")) {
        # Get predicted values
        preds <- predict(model, newdata = miniem, allow.new.levels = TRUE)
        # Extract standard deviation of prediction uncertainty
        uncert <- merTools::predictInterval(model, newdata = miniem, level = 0.95, 
                                            n.sims = 1000, which = "all", include.resid.var = TRUE)
        # The standard deviation is estimated from the posterior distribution
        sddies <- (uncert$upr - uncert$lwr) / (2 * 1.96)
        # Create output dataframe
        sevvies%<>%rbind(cbind(miniem,data.frame(
          mu = as.numeric(preds),
          sd = as.numeric(sddies),
          dep = "crops")))
      } else {
        stop("Model failed to fit; cannot generate crop predictions.")
      }
    }
    # Remove any duplicated event predictions
    sevvies%<>%filter(!is.na(mu) & !is.na(sd))%>%
      arrange(sd)%>%distinct(pick(c(disno,dep)),.keep_all = T)
    # Make sure to convert back the conflict hazard codes
    sevvies$haz_grp[sevvies$haz_Ab=="CF"]<-"CF"
    # Check all events from emdat are in the predictions
    length(unique(sevvies$disno[sevvies$dep=="cattle"]))
    length(unique(sevvies$disno[sevvies$dep=="crops"]))
    sum(!emdat$disno%in%unique(sevvies$disno[sevvies$dep=="cattle"]))
    sum(!emdat$disno%in%unique(sevvies$disno[sevvies$dep=="crops"]))
    sum(apply(emdat[!emdat$disno%in%unique(sevvies$disno[sevvies$dep=="cattle"]),impact_vars],1,function(x) any(is.na(x))))
    sum(apply(emdat[!emdat$disno%in%unique(sevvies$disno[sevvies$dep=="crops"]),impact_vars],1,function(x) any(is.na(x))))
    
    p<-sevvies%>%
      pivot_longer(cols = impact_vars[1:3], values_to = "values",names_to = "impact")%>%
      ggplot()+geom_point(aes(exp(values),exp(mu)),alpha=0.1)+
      geom_smooth(aes(exp(values),exp(mu),colour=impact),se =F )+
      xlab("Observed Impact")+ylab("Predicted Impact")+
      scale_x_log10(
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))
      ) +
      scale_y_log10(
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))
      ) +
      theme_bw()+
      facet_wrap(~dep+impact,scales = "free");p
    ggsave("EMDAT_DisSev_Corrplot.png",p,path="./Plots",width=12,height=10)
    
    sevvies[,c("mu",impact_vars[1:3])]%>%pairs()
      
    sevvies$mu[sevvies$mu>25]<-25
    
    sevvies$proportion<-sevvies$mu/sevvies$surfarea
    
    p<-sevvies%>%ggplot()+
      geom_point(aes(exp(mu),proportion),alpha=0.1)+
      ylab("Proportion of Country Area")+xlab("Predicted Impact")+
      scale_x_log10(
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))
      ) +
      scale_y_log10(
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))
      ) +
      theme_bw();p
    ggsave("EMDAT_DisSev_Proportion_Corrplot.png",p,path="./Plots",width=7,height=5)
    
    sevvies%>%filter(!is.infinite(mu) & !is.infinite(sd) & !is.na(mu) & !is.na(sd))%>%
      return()
  }
} else { # infer disaster severity from EM-DAT impact types directly
  stop("EM-DAT-based disaster severity model not ready yet")
  GetDisSev<-function(dissie,emdat){
    # 
  }
}





