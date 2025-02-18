
if(Desinventar){ # infer disaster severity from Desinventar data
  # Extract the disaster severity estimates
  GetDisSev<-function(dissie){
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
    
    # Define independent variables
    imp_sets <- list(
      c("deaths", "cost", "affected"),
      c("norm_deaths", "norm_cost", "norm_affected"))
    # Define fixed and random effect variables
    fixed_effects_all <- c("region","haz_grp")
    # Random effect for income group
    random_effects_all <- c("(1 | duration)","(1 | incomegrp)")
    stop("Make incomegrp ordinal")
    # Combinations of all possible model formulations
    generate_subsets <- function(vars) {
      unlist(lapply(1:length(vars), function(i) combinat::combn(vars, i, simplify = FALSE)), recursive = FALSE)
    }
    impact_combinations <- generate_subsets(unique(unlist(imp_sets))) # Impact variables subsets
    fixed_combinations <- generate_subsets(fixed_effects_all) # Fixed effects subsets
    random_combinations <- generate_subsets(random_effects_all) # Random effects subsets
    # Define dependent variables
    dep_vars <- c("crops", "cattle", "cbind(crops,cattle)")
    
    for(deppie in dep_vars){
      print(deppie)
      # Function to create model formula
      F_model <- function(impact_vars, fixed_vars, random_vars, deppie="crops") {
        fixed_formula <- paste(c(fixed_vars, impact_vars), collapse = " + ")
        random_formula <- paste(random_vars, collapse = " + ")
        formula_str <- paste0(deppie," ~ ", fixed_formula, " + ", random_formula)
        return(as.formula(formula_str))
      }
      # Fit models and store results
      mod_res <- list()
      for (impact_vars in impact_combinations) {
        for (fixed_vars in fixed_combinations) {
          for (random_vars in random_combinations) {
            # Ensure only complete records are modelled
            df <- dissie %>% filter(complete.cases(select(., all_of(c(impact_vars, fixed_vars, 
                                                                      str_replace_all(str_replace_all(random_vars,"\\(1 \\| ",""),"\\)",""))))))
            # Ensure enough data for modeling
            if (nrow(df) > 30) {  
              # Model formula
              formula <- F_model(impact_vars, fixed_vars, random_vars, deppie)
              # Run the model
              model <- try(lme4::lmer(formula = formula, data = df, REML = FALSE), silent = TRUE)
              # Store results if successful
              if (!inherits(model, "try-error")) {
                mod_res[[paste0(as.character(formula)[-1],collapse = " ~ ")]] <- list(
                  model = model,
                  dep_var=deppie,
                  formula = paste0(formula, collapse = " "),
                  BIC = BIC(model),
                  AIC = AIC(model),
                  n = nrow(df)
                )
              } else {
                print(paste0("Failed model: ", paste(impact_vars, collapse = ", "), " | ", paste(fixed_vars, collapse = ", "), " | ", paste(random_vars, collapse = ", ")))
              }
            }
          }
        }
      }
    }
    
    # Compare models by BIC
    bench <- data.frame(
      impact_set = names(mod_res),
      formula = sapply(mod_res, function(x) x$formula),
      BIC = sapply(mod_res, function(x) x$BIC),
      AIC = sapply(mod_res, function(x) x$AIC),
      n = sapply(mod_res, function(x) x$n)
    ) %>% arrange(BIC)  # Sort by BIC
    # Display the model comparison results
    View(bench)
    
    # Output the best model
    return(mod_res[[which.max(bench$BIC)]]$model)
    
  }
} else { # infer disaster severity from EM-DAT impact types directly
  stop("EM-DAT-based disaster severity model not ready yet")
  GetDisSev<-function(dissie){
    # 
  }
}











