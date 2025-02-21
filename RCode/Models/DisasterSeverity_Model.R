
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
    num_folds <- 10
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
    dep_vars <- c("crops", "cattle", "norm_crops", "norm_cattle") #, "cbind(crops,cattle)")
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
      "DZ"="OT"),
    c("CW"="OT",
      "DR"="OT",
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
      "DZ"="OT"),
    c("CW"="OT",
      "DR"="OT",
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
      "DZ"="OT"),
    c("CW"="OT",
      "DR"="OT",
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
      "DZ"="OT"))
    # Function to create a random effect
    Randeff<-function(x) paste0("(1 | ",x,")",collapse = " + ")
    # Define weighting factors to iterate over
    weighting_factors <- c(0,0.5,1)
    # Dataframe template
    mod_res <- data.frame()
    # Run it!
    for(ww in weighting_factors){
      # Different hazard grouping models
      for(j in 1:length(hazgrps)){
        # Try changing the number of hazard groups
        dissie%<>%GroupHazs(hazgrps[[j]])
        # For each dependent variable to be modelled
        for(deppie in dep_vars){
          print(deppie)
          # Function to create model formula
          F_model <- function(impact_vars, fixed_vars, random_vars, deppie="crops") {
            fixed_formula <- paste(fixed_vars, collapse = " + ")
            random_formula <- paste(Randeff(c(impact_vars,random_vars)), collapse = " + ")
            formula_str <- paste0(deppie," ~ ", fixed_formula, " + ", random_formula)
            return(as.formula(formula_str))
          }
          # Fit models and store results
          for (impact_vars in impact_combinations) {
            for (fixed_vars in fixed_combinations) {
              for (random_vars in random_combinations) {
                # Ensure only complete records are modelled
                df <- dissie %>% filter(complete.cases(select(., all_of(c(impact_vars, fixed_vars, random_vars)))))%>%
                  calculate_weights(factor = ww)
                # Ensure enough data for modeling
                if (nrow(df) > 30) {  
                  # Ensure reproducibility
                  set.seed(123)
                  # Create 10 CV folds
                  df$fold <- createFolds(df$weight, k = num_folds, list = FALSE)
                  # Model formula
                  formula <- F_model(impact_vars, fixed_vars, random_vars, deppie)
                  # Store BIC and AIC for each fold
                  bic_values <- aic_values <- c()
                  # Go through folds
                  for (fold in 1:num_folds) {
                    # Training and testing split
                    train_data <- df %>% filter(fold != fold)
                    test_data <- df %>% filter(fold == fold)
                    # Run the model
                    model <- suppressWarnings(try(lme4::lmer(formula = formula, data = train_data, REML = TRUE, verbose = FALSE, weights = train_data$weight), silent = TRUE))
                    # Store results if successful
                    if (!inherits(model, "try-error")) {
                      bic_values <- c(bic_values, BIC(model))
                      aic_values <- c(aic_values, AIC(model))
                    } else {
                      print(paste0("Failed model at fold ", fold, ": ", paste(impact_vars, collapse = ", "), " | ", paste(fixed_vars, collapse = ", "), " | ", paste(random_vars, collapse = ", ")))
                    }
                  }
                  # Store average performance across folds
                  mod_res %<>% rbind(data.frame(
                    dep_var = deppie,
                    weight_fac = ww,
                    hazgrp = j,
                    formula = paste0(as.character(formula)[2:3], collapse = " ~ "),
                    BIC = mean(bic_values),
                    AIC = mean(aic_values),
                    n = nrow(df)
                  ))
                }
              }
            }
          }
        }
      }
    }
    # Display the model comparison results
    View(mod_res)
    
    
    
  }
} else { # infer disaster severity from EM-DAT impact types directly
  stop("EM-DAT-based disaster severity model not ready yet")
  GetDisSev<-function(dissie){
    # 
  }
}




###### TODAY ######
# Add conflict data - use quantiles then normalise to range of disaster severity values of disaster data
# Extract models depending on which data is present
# Calculate adj-R-sq of predictions vs observations
# CONVERT FROM HECTARES TO TONNES
# Develop the function to apply the models to the input data from EM-DAT
# Generate mu+sigma of log-plot
# Exponentiate sampled values in stan
# Modify Stan to accept all commodities and not just crops or cattle
# Run stan code on magpie
# Model normalised crop and cattle losses using GLM
# Add datasets into stan file to calculate the losses in production in USD?




