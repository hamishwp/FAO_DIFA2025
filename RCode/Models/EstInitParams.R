# params <- list(beta_dur = 1,
#                beta_dis = -1e-1,
#                hsev=rep(1,fdf$n_haz),
#                isev=rep(1,fdf$n_com),
#                beta_y1=rep(1,fdf$n_com),
#                sigma=1)

# Calculate the default model likelihood - log production values
m_likelihood <- function(fdf, params) {
  # Initialise log-likelihood
  loglk<-0
  # Iterate over countries
  for (iso in 1:fdf$n_isos) {
    # Save on computation
    lnmu_AR1 <- fdf$lnmu_AR1[iso, ]
    lnsig_AR1 <- fdf$lnsig_AR1[iso, ]
    # Iterate over time
    for (ttt in 2:fdf$n_t) {
      if(sum(fdf$flag[iso, ttt, 1:fdf$n_dis[iso]])!=0 & sum(fdf$iprox[iso, 1:fdf$n_dis[iso], ])!=0){
        # Calculate the disaster severity
        dsev <- sapply(1:fdf$n_com,function(k){
          log(10+sum(exp(fdf$iprox[iso, 1:fdf$n_dis[iso], k]) * 
                       fdf$flag[iso, ttt, 1:fdf$n_dis[iso]] * 
                       params$hsev[fdf$htype[iso, 1:fdf$n_dis[iso]]])) # * fdf$hazdur[iso, ttt, 1:fdf$n_dis[iso]]))
        })
      } else dsev <- rep(0,fdf$n_com)
      # Compute GPR mean function
      mu <- params$beta_dis * dsev * params$isev +
        params$beta_y1 * lnmu_AR1 * fdf$lny[iso, ttt-1, ]
      # Compute adjusted sigma for AR1
      red_sig <- lnsig_AR1 * params$sigma
      # Log-likelihood
      loglk<-loglk+sum(dnorm(fdf$lny[iso, ttt, ],
                             mu,red_sig,log = T))
    }
  }
  
  return(loglk)
}

# Calculate the default model likelihood - log production values
m_likelihood_bin <- function(fdf, params) {
  # Initialise log-likelihood
  loglk<-0
  # Iterate over countries
  for (iso in 1:fdf$n_isos) {
    # Save on computation
    lnmu_AR1 <- fdf$lnmu_AR1[iso, ]
    lnsig_AR1 <- fdf$lnsig_AR1[iso, ]
    # Iterate over time
    for (ttt in 2:fdf$n_t) {
      # if(sum(fdf$flag[iso, ttt, 1:fdf$n_dis[iso]])!=0 & sum(fdf$iprox[iso, 1:fdf$n_dis[iso], k])!=0){
      #   # Calculate the disaster severity
      #   dsev <- sapply(1:fdf$n_com,function(k){
      #     log(10+sum(exp(fdf$iprox[iso, 1:fdf$n_dis[iso], k]) * fdf$flag[iso, ttt, 1:fdf$n_dis[iso]] * params$hsev[fdf$htype[iso, 1:fdf$n_dis[iso]]])) # * fdf$hazdur[iso, ttt, 1:fdf$n_dis[iso]]))
      #   })
      # } else dsev <- rep(0,fdf$n_com)
      if(sum(fdf$flag[iso, ttt, 1:fdf$n_dis[iso]])!=0 & sum(fdf$iprox[iso, 1:fdf$n_dis[iso], ])!=0){
        # Calculate the disaster severity
        dsev<-sapply(1:fdf$n_com, function(k) mean(as.integer(fdf$iprox[iso, 1:fdf$n_dis[iso], k]>0) * params$hsev[fdf$htype[iso, 1:fdf$n_dis[iso]]]))
      } else dsev <- rep(0,fdf$n_com)
      # Compute GPR mean function
      mu <- params$beta_dis * dsev * params$isev +
        params$beta_y1 * lnmu_AR1 * fdf$lny[iso, ttt-1, ]
      # Compute adjusted sigma for AR1
      red_sig <- lnsig_AR1 * params$sigma
      # Log-likelihood
      loglk<-loglk+sum(dnorm(fdf$lny[iso, ttt, ],
                             mu,red_sig,log = T))
      if(is.na(sum(dnorm(fdf$lny[iso, ttt, ],
                         mu,red_sig,log = T)))) stop("warning")
    }
  }
  
  return(loglk)
}

# Calculate the default model likelihood - log production values
m_likelihood_hazdur <- function(fdf, params) {
  # Initialise log-likelihood
  loglk<-0
  # Iterate over countries
  for (iso in 1:fdf$n_isos) {
    # Save on computation
    lnmu_AR1 <- fdf$lnmu_AR1[iso, ]
    lnsig_AR1 <- fdf$lnsig_AR1[iso, ]
    # Iterate over time
    for (ttt in 2:fdf$n_t) {
      if(sum(fdf$flag[iso, ttt, 1:fdf$n_dis[iso]])!=0 & sum(fdf$iprox[iso, 1:fdf$n_dis[iso], ])!=0){
        # Calculate the disaster severity
        dsev <- sapply(1:fdf$n_com,function(k){
          log(10+sum(exp(fdf$iprox[iso, 1:fdf$n_dis[iso], k]) * (1 + params$beta_dur*fdf$hazdur[iso,ttt,1:fdf$n_dis[iso]]) *
                           fdf$flag[iso, ttt, 1:fdf$n_dis[iso]] * params$hsev[fdf$htype[iso, 1:fdf$n_dis[iso]]])) # * fdf$hazdur[iso, ttt, 1:fdf$n_dis[iso]]))
        })
      } else dsev <- rep(0,fdf$n_com)
      # Compute GPR mean function
      mu <- params$beta_dis * dsev * params$isev +
        params$beta_y1 * lnmu_AR1 * fdf$lny[iso, ttt-1, ]
      # Compute adjusted sigma for AR1
      red_sig <- lnsig_AR1 * params$sigma
      # Log-likelihood
      loglk<-loglk+sum(dnorm(fdf$lny[iso, ttt, ],
                             mu,red_sig,log = T))
    }
  }
  
  return(loglk)
}

# Predict the production values for the default model
predict_y <- function(fdf, params) {
  # Initialise production data
  y <- fdf$lny
  # Iterate over countries
  for (iso in 1:fdf$n_isos) {
    # Save on computation
    hs <- params$hsev[fdf$htype[iso, 1:fdf$n_dis[iso]]]
    iproxhs <- fdf$iprox[iso, 1:fdf$n_dis[iso], ] / hs
    if(fdf$n_dis[iso] == 1) iproxhs%<>%matrix(nrow=1)
    lnmu_AR1 <- fdf$lnmu_AR1[iso, ]
    lnsig_AR1 <- fdf$lnsig_AR1[iso, ]
    # Iterate over time
    for (ttt in 2:fdf$n_t) {
      # Extract flag for disaster impact
      flag_mat <- matrix(fdf$flag[iso, ttt, 1:fdf$n_dis[iso]],nrow=fdf$n_dis[iso],ncol=1)
      # Disaster severity calculation
      if (sum(flag_mat) > 0) {
        # calculate the severity
        dsev <- t(iproxhs) %*% (flag_mat * (
          hs * fdf$hazdur[iso, ttt, 1:fdf$n_dis[iso]] * params$beta_dur +
            exp(-fdf$ts[iso, ttt, 1:fdf$n_dis[iso]] / hs) -
            exp(-fdf$tf[iso, ttt, 1:fdf$n_dis[iso]] / hs)))
      } else dsev <-rep(0,fdf$n_com)
      # Compute GPR mean function
      mu <- params$beta_dis * dsev * params$isev +
        params$beta_y1 * lnmu_AR1 * fdf$lny[iso, ttt-1, ]
      # Compute adjusted sigma for AR1
      red_sig <- lnsig_AR1 * params$sigma
      # Log-likelihood
      # y[iso, ttt, , ] <- exp(rnorm(30,mu,red_sig,log = T))
      y[iso, ttt, ] <- exp(mu)
    }
  }
  
  return(y)
}

# Calculate the default model likelihood - log production values
predict_y_bin <- function(fdf, params, mxdis=1) {
  # Initialise log-likelihood
  loglk<-0
  fdf$n_dis<-pmin(mxdis,fdf$n_dis)
  ynodis<-ydis<-dsevvie<-fdf$lny
  dsevvie[]<-0
  # Iterate over countries
  for (iso in 1:fdf$n_isos) {
    # Save on computation
    lnmu_AR1 <- fdf$lnmu_AR1[iso, ]
    lnsig_AR1 <- fdf$lnsig_AR1[iso, ]
    # Iterate over time
    for (ttt in 2:fdf$n_t) {
      # if(sum(fdf$flag[iso, ttt, 1:fdf$n_dis[iso]])!=0 & sum(fdf$iprox[iso, 1:fdf$n_dis[iso], k])!=0){
      #   # Calculate the disaster severity
      #   dsev <- sapply(1:fdf$n_com,function(k){
      #     log(10+sum(exp(fdf$iprox[iso, 1:fdf$n_dis[iso], k]) * fdf$flag[iso, ttt, 1:fdf$n_dis[iso]] * params$hsev[fdf$htype[iso, 1:fdf$n_dis[iso]]])) # * fdf$hazdur[iso, ttt, 1:fdf$n_dis[iso]]))
      #   })
      # } else dsev <- rep(0,fdf$n_com)
      if(sum(fdf$flag[iso, ttt, 1:fdf$n_dis[iso]])!=0 & sum(fdf$iprox[iso, 1:fdf$n_dis[iso], ])!=0){
        # Calculate the disaster severity
        dsev<-sapply(1:fdf$n_com, function(k) mean(as.integer(fdf$iprox[iso, 1:fdf$n_dis[iso], k]>0) * params$hsev[fdf$htype[iso, 1:fdf$n_dis[iso]]]))
      } else dsev <- rep(0,fdf$n_com)
      # Calculate disaster commodity data
      ydis[iso, ttt-1, ] <- params$beta_dis * dsev * params$isev +
        params$beta_y1 * lnmu_AR1 * fdf$lny[iso, ttt-1, ]
      # Calculate no-disaster commodity data
      ynodis[iso, ttt-1, ] <- params$beta_y1 * lnmu_AR1 * fdf$lny[iso, ttt-1, ]
      # Disaster severity
      dsevvie[iso, ttt-1, ] <- dsev
    }
  }
  
  return(list(ydis=ydis,ynodis=ynodis,ydiff=exp(ynodis)-exp(ydis),dsevvie=dsevvie))
}

# Calculate the default model likelihood - log production values
predloss_bin <- function(fdf, params, mxdis=1, n_sam=1000) {
  # Initialise log-likelihood
  loglk<-0
  fdf$n_dis<-pmin(mxdis,fdf$n_dis)
  losses <- array(0,dim=c(fdf$n_isos, fdf$n_t, fdf$n_com, n_sam))
  all_losses <- array(0,dim=c(fdf$n_com, n_sam))
  # Compute gamma parameters to sample the price commodity data
  alpha <- fdf$mu_prices^2 / fdf$sig_prices^2 # scale parameter
  lambda <- fdf$mu_prices / fdf$sig_prices^2  # rate parameter
  # Iterate over countries
  for (iso in 1:fdf$n_isos) {
    # Save on computation
    lnmu_AR1 <- fdf$lnmu_AR1[iso, ]
    lnsig_AR1 <- fdf$lnsig_AR1[iso, ]
    # Iterate over time
    for (ttt in 2:fdf$n_t) {
      if(sum(fdf$flag[iso, ttt, 1:fdf$n_dis[iso]])!=0 & sum(fdf$iprox[iso, 1:fdf$n_dis[iso], ])!=0){
        # Calculate the disaster severity
        dsev<-sapply(1:fdf$n_com, function(k) mean(as.integer(fdf$iprox[iso, 1:fdf$n_dis[iso], k]>0) * params$hsev[fdf$htype[iso, 1:fdf$n_dis[iso]]]))
        for(k in 1:fdf$n_com){
          # Production estimates
          ydis<-params$beta_dis * dsev[k] * params$isev[k] +
            params$beta_y1 * lnmu_AR1[k] * fdf$lny[iso, ttt-1, k]
          ynodis<-params$beta_y1 * lnmu_AR1[k] * fdf$lny[iso, ttt-1, k]
          # Losses
          losses[iso, ttt, k, ] <- (exp(ynodis)-exp(ydis))*
            rgamma(n_sam, shape = alpha[iso, ttt, k], rate = lambda[iso, ttt, k])
          # Aggregated losses
          all_losses[k, ] <- sapply(1:n_sam,function(s) sum(losses[, , k, s])) + all_losses[k, ]
        }
      } 
      # Calculate the losses per commodity
    }
  }
  
  return(list(losses=losses,all_losses=all_losses))
}

# Parallelised version of the above function to predict commodity losses
predloss_bin_par <- function(fdf, params, mxdis = 1, n_sam = 1000, n_cores = 4) {
  # Reduce the number of disasters to max mxdis
  fdf$n_dis <- pmin(mxdis, fdf$n_dis)
  # Precompute gamma parameters
  alpha <- fdf$mu_prices^2 / fdf$sig_prices^2  # shape parameter
  lambda <- fdf$mu_prices / fdf$sig_prices^2  # rate parameter
  # Function to compute for a single country
  compute_country_losses <- function(iso) {
    lnmu_AR1 <- fdf$lnmu_AR1[iso, ]
    lnsig_AR1 <- fdf$lnsig_AR1[iso, ]
    # Store country-specific losses
    country_losses <- array(0, dim = c(fdf$n_t, fdf$n_com, n_sam))
    country_all_losses <- matrix(0, nrow = fdf$n_com, ncol = n_sam)
    # Iterate over years
    for (ttt in 2:fdf$n_t) {
      if (sum(fdf$flag[iso, ttt, 1:fdf$n_dis[iso]]) != 0 &&
          sum(fdf$iprox[iso, 1:fdf$n_dis[iso], ]) != 0) {
        # Calculate disaster severity efficiently
        dsev <- sapply(1:fdf$n_com, function(k) {
          mean(as.integer(fdf$iprox[iso, 1:fdf$n_dis[iso], k] > 0) *
                 params$hsev[fdf$htype[iso, 1:fdf$n_dis[iso]]])
        })
        # Iterate over commodities
        for (k in 1:fdf$n_com) {
          # Production with and without disaster
          ydis <- params$beta_dis * dsev[k] * params$isev[k] +
            params$beta_y1 * lnmu_AR1[k] * fdf$lny[iso, ttt - 1, k]
          ynodis <- params$beta_y1 * lnmu_AR1[k] * fdf$lny[iso, ttt - 1, k]
          # Sample production value with gamma uncertainty
          gamma_samples <- rgamma(n_sam, shape = alpha[iso, ttt, k], rate = lambda[iso, ttt, k])
          # Compute losses
          loss_values <- (exp(ynodis) - exp(ydis)) * gamma_samples
          country_losses[ttt, k, ] <- loss_values
          # Aggregate losses
          country_all_losses[k, ] <- country_all_losses[k, ] + loss_values
        }
      }
    }
    
    list(country_losses = country_losses, country_all_losses = country_all_losses)
  }
  # Parallelise and run
  country_results <- parallel::mclapply(1:fdf$n_isos, compute_country_losses, mc.cores = ncores)
  
  # Combine results back into arrays
  losses <- array(0, dim = c(fdf$n_isos, fdf$n_t, fdf$n_com, n_sam))
  all_losses <- matrix(0, nrow = fdf$n_com, ncol = n_sam)
  
  for (iso in 1:fdf$n_isos) {
    losses[iso, , , ] <- country_results[[iso]]$country_losses
    all_losses <- all_losses + country_results[[iso]]$country_all_losses
  }
  
  return(list(losses = losses, all_losses = all_losses))
}


simulate_fdf <- function(fdf, params) {
  # Initialise simulated lny data with the real first values
  lny_sim <- array(NA, dim = c(fdf$n_isos, fdf$n_t, fdf$n_com))
  lny_sim[, 1, ] <- fdf$lny[, 1, ]  # Keep real first year values
  # Iterate over countries
  for (iso in 1:fdf$n_isos) {
    # Save on computation
    lnmu_AR1 <- fdf$lnmu_AR1[iso, ]
    lnsig_AR1 <- fdf$lnsig_AR1[iso, ]
    # Iterate over time
    for (ttt in 2:fdf$n_t) {
      if(sum(fdf$flag[iso, ttt, 1:fdf$n_dis[iso]])!=0 & sum(fdf$iprox[iso, 1:fdf$n_dis[iso], ])!=0){
        # Calculate the disaster severity
        dsev <- sapply(1:fdf$n_com,function(k){
          log(10+sum(exp(fdf$iprox[iso, 1:fdf$n_dis[iso], k]) * (1 + params$beta_dur*fdf$hazdur[iso,ttt,1:fdf$n_dis[iso]]) *
                       fdf$flag[iso, ttt, 1:fdf$n_dis[iso]] * params$hsev[fdf$htype[iso, 1:fdf$n_dis[iso]]])) # * fdf$hazdur[iso, ttt, 1:fdf$n_dis[iso]]))
        })
      } else dsev <- rep(0,fdf$n_com)
      # Compute AR1 mean function (AR1 process)
      mu <- params$beta_dis * dsev * params$isev +
        params$beta_y1 * lnmu_AR1 * lny_sim[iso, ttt - 1, ]
      # Compute adjusted sigma for AR1
      red_sig <- lnsig_AR1 * params$sigma
      # Simulate next time step from normal distribution
      lny_sim[iso, ttt, ] <- rnorm(fdf$n_com, mean = mu, sd = red_sig)
    }
  }
  
  return(lny_sim)
}



estimate_gp_params <- function(y_data, time_points, priors=T) {
  # Create data frame with time and response variable
  df <- data.frame(time = time_points + runif(length(time_points), 0, 1e-6), y = y_data)
  # Fit a Gaussian Process model using an RBF-like correlation structure
  gp_model <- nlme::gls(y ~ 1, data = df, correlation = nlme::corExp(form = ~ time))
  # Extract estimated parameters
  rho_hat <- 1 / coef(gp_model$modelStruct$corStruct, unconstrained = FALSE)  # Length-scale (ρ)
  alpha_hat <- sqrt(var(residuals(gp_model)))  # Marginal standard deviation (α)
  
  if(priors) return(list(alpha = alpha_hat, rho = rho_hat))
  
  # # Fit a Gaussian Process using an exponentiated quadratic (RBF) kernel
  # gp_model <- DiceKriging::km(design = as.data.frame(time_points), response = y_data,
  #                covtype = "gauss", nugget.estim = TRUE)
  # # Extract MLE estimates
  # alpha_hat <- sqrt(gp_model@covariance@sd2)  # Marginal standard deviation (α)
  # rho_hat <- sqrt(gp_model@covariance@range)  # Length-scale (ρ)
  # # If we don't need the rest, save on computation
  # if(priors) return(list(alpha = alpha_hat, rho = rho_hat))
  # # Extract standard errors of MLE estimates
  # alpha_se <- sqrt(gp_model@covariance@sd2Var)  # Standard error of α
  # rho_se <- sqrt(gp_model@covariance@rangeVar)  # Standard error of ρ
  # # Fit Gamma distributions
  # shape_alpha <- (alpha_hat / alpha_se)^2
  # scale_alpha <- (alpha_se^2) / alpha_hat
  # shape_rho <- (rho_hat / rho_se)^2
  # scale_rho <- (rho_se^2) / rho_hat
  # 
  # return(list(
  #   alpha = alpha_hat, se_alpha = alpha_se, shape_alpha = shape_alpha, scale_alpha = scale_alpha,
  #   rho = rho_hat, se_rho = rho_se, shape_rho = shape_rho, scale_rho = scale_rho
  # ))
}

InitParams<-function(fdf, iprox_dat=T, GPR=F, empAR=F){
  
  warning("FUMBLING INITIAL VALUES FOR NOW")
  
  return(function(chainnum){
    list(beta_dis = 0,
         beta_dur = 1,
         hsev = rep(1,fdf$n_haz),
         isev = rep(1,fdf$n_com),
         beta_muAR1 = 1,
         beta_sigAR1 = 0.99)
  })
  
  
  
  
  
  
  
  
  
  
  
  
  if(empAR) {
    # Estimate variance in AR1 residuals
    y_p <- fdf$y; y_p[]<-0
    for(i in 1:dim(fdf$y)[1]){
      for(j in 1:dim(fdf$y)[3]){
        for(k in 2:dim(fdf$y)[2]){
          y_p[i,k,j]<-(fdf$y[i,k,j] - fdf$mu_AR1[i,j]*fdf$y[i,k-1,j])/fdf$sig_AR1[i,j]
        }
      }
    }
    sigres_AR <- sd(c(y_p)[abs(y_p)>1e-6],na.rm = T)
    
    return(function(chainnum){
      list(beta_dis = 0,
           beta_dur = 1,
           hsev = rep(1,fdf$n_haz),
           isev = rep(1,fdf$n_com),
           gamAR1 = 1,
           sdAR1 = sigres_AR)
    })
  }
  if(!GPR) {
    # Estimate variance in AR1 residuals
    y_p <- fdf$y; y_p[]<-0; sigres_AR<-fdf$sig_AR1; sigres_AR[]<-1e-6
    for(i in 1:dim(fdf$y)[1]){
      for(j in 1:dim(fdf$y)[3]){
        for(k in 2:dim(fdf$y)[2]){
          y_p[i,k,j]<-(fdf$y[i,k,j] - fdf$mu_AR1[i,j]*fdf$y[i,k-1,j])/fdf$sig_AR1[i,j]
        }
        sigres_AR[i,j] <- pmax(1e-6,sd(y_p[i,,j],na.rm = T))
      }
    }
    
    if(iprox_dat){
      return(function(chainnum){
        list(iprox = fdf$mu_dis,
             beta_dis = 0,
             beta_dur = 1,
             hsev = rep(1,fdf$n_haz),
             isev = rep(1,fdf$n_com),
             beta_y1 = fdf$mu_AR1,
             sigma = sigres_AR)
      })
    } else {
      return(function(chainnum){
        list(beta_y1 = fdf$mu_AR1,
             beta_dis = 0,
             beta_dur = 1,
             hsev = rep(1,fdf$n_haz),
             isev = rep(1,fdf$n_com),
             sigma = sigres_AR)
      })
    }
  }
  # Storage for estimated parameters
  alpha_est <- rep(NA,fdf$n_isos)
  rho_est <- rep(NA,fdf$n_isos)
  # Fit GPR per country per commodity
  for (iso in 1:fdf$n_isos) {
    # Convert fdf$y (array) into a tidy data frame
    y <- as.data.frame.table(fdf$y[iso,,]) %>%
      rename(time_index = Var1, commodity_index = Var2, y_value = Freq) %>%
      mutate(time = fdf$time[as.numeric(time_index)],  # Map correct time values
             commodity_index = as.numeric(commodity_index))%>%  # Convert to numeric if needed
      dplyr::select(time, commodity_index, y_value)  # Reorder c
    # Fit Gaussian Process & extract parameters
    gp_params <- estimate_gp_params(y$y_value, y$time, priors=T)
    # Store estimates
    alpha_est[iso] <- gp_params$alpha
    rho_est[iso] <- gp_params$rho
  }
  # Output the prior information to check we aren't too far off with our estimates
  print(paste0("Alpha values range from : ",paste0(round(range(alpha_est,na.rm = T)),collapse = " to ")))
  print(paste0("Rho values range from : ",paste0(round(range(rho_est,na.rm = T)),collapse = " to ")))
  # Are we sampling iprox or not? If not, we don't need to provide iprox
  if(iprox_dat){
    return(function(chainnum){
      list(iprox = fdf$mu_dis,
           hsev = rep(1,fdf$n_haz),
           isev = rep(1,fdf$n_com),
           beta_dis = 0,
           beta_dur = 1,
           rho = rho_est,
           alpha = alpha_est,
           beta_y1 = fdf$mu_AR1)
    })
  } else {
    return(function(chainnum){
      list(rho = rho_est,
           alpha = alpha_est,
           hsev = rep(1,fdf$n_haz),
           isev = rep(1,fdf$n_com),
           beta_dis = 0,
           beta_dur = 1,
           beta_y1 = fdf$mu_AR1)
    })
  }
}











# InitParams<-function(fdf, iprox_dat=T){
#   # Storage for estimated parameters
#   alpha_est <- matrix(NA, nrow = fdf$n_isos, ncol = fdf$n_com)
#   rho_est <- matrix(NA, nrow = fdf$n_isos, ncol = fdf$n_com)
#   alpha_shape <- matrix(NA, nrow = fdf$n_isos, ncol = fdf$n_com)
#   alpha_scale <- matrix(NA, nrow = fdf$n_isos, ncol = fdf$n_com)
#   rho_shape <- matrix(NA, nrow = fdf$n_isos, ncol = fdf$n_com)
#   rho_scale <- matrix(NA, nrow = fdf$n_isos, ncol = fdf$n_com)
#   # Fit GPR per country per commodity
#   for (iso in 1:fdf$n_isos) {
#     for (com in 1:fdf$n_com) {
#       # Fit Gaussian Process & extract parameters
#       gp_params <- estimate_gp_params(fdf$y[iso, , com], fdf$time, priors=T)
#       # Store estimates
#       alpha_est[iso, com] <- gp_params$alpha
#       alpha_shape[iso, com] <- gp_params$shape_alpha
#       alpha_scale[iso, com] <- gp_params$scale_alpha
#       rho_est[iso, com] <- gp_params$rho
#       rho_shape[iso, com] <- gp_params$shape_rho
#       rho_scale[iso, com] <- gp_params$scale_rho
#     }
#   }
#   # Output the prior information to check we aren't too far off with our estimates
#   print(paste0("Alpha scale values range from : ",min(alpha_scale,na.rm = T)))
#   print(paste0("Alpha shape values range from : ",min(alpha_shape,na.rm = T)))
#   print(paste0("Rho scale values range from : ",min(rho_scale,na.rm = T)))
#   print(paste0("Rho shape values range from : ",min(rho_shape,na.rm = T)))
#   # Are we sampling iprox or not? If not, we don't need to provide iprox
#   if(iprox_dat){
#     return(function(chainnum){
#       list(iprox = fdf$mu_dis,
#            rho = rho_est,
#            alpha = alpha_est,
#            beta_y1 = fdf$mu_AR1)
#     })
#   } else {
#     return(function(chainnum){
#       list(rho = rho_est,
#            alpha = alpha_est,
#            beta_y1 = fdf$mu_AR1)
#     })
#   }
# }


# # Calculate for all commodities summed into one total production
# m_likelihood_1D <- function(fdf, params) {
#   # Initialise log-likelihood
#   loglk<-0
#   # Iterate over countries
#   for (iso in 1:fdf$n_isos) {
#     # Save on computation
#     hs <- params$hsev[fdf$htype[iso, 1:fdf$n_dis[iso]]]
#     iproxhs <- fdf$iprox[iso, 1:fdf$n_dis[iso], ] / hs
#     if(fdf$n_dis[iso] == 1) iproxhs%<>%matrix(nrow=1)
#     lnmu_AR1 <- fdf$lnmu_AR1[iso, ]
#     lnsig_AR1 <- fdf$lnsig_AR1[iso, ]
#     # Iterate over time
#     for (ttt in 2:fdf$n_t) {
#       # Extract flag for disaster impact
#       flag_mat <- matrix(fdf$flag[iso, ttt, 1:fdf$n_dis[iso]],nrow=fdf$n_dis[iso],ncol=1)
#       # Disaster severity calculation
#       if (sum(flag_mat) > 0) {
#         # calculate the severity
#         dsev <- t(iproxhs) %*% (flag_mat * (
#           hs * fdf$hazdur[iso, ttt, 1:fdf$n_dis[iso]] * params$beta_dur +
#             exp(-fdf$ts[iso, ttt, 1:fdf$n_dis[iso]] / hs) -
#             exp(-fdf$tf[iso, ttt, 1:fdf$n_dis[iso]] / hs)))
#       } else dsev <-rep(0,fdf$n_com)
#       # Compute GPR mean function
#       mu <- params$beta_dis * dsev * params$isev +
#         params$beta_y1 * lnmu_AR1 * fdf$lny[iso, ttt-1, ]
#       # Compute adjusted sigma for AR1
#       red_sig <- lnsig_AR1 * params$sigma
#       # Log-likelihood
#       loglk<-loglk+sum(dnorm(sum(fdf$lny[iso, ttt, ]),
#                              sum(mu),mean(red_sig),log = T))
#     }
#   }
#   
#   return(loglk)
# }


# Calculate the default model likelihood - log production values
# m_likelihood_old <- function(fdf, params) {
#   # Initialise log-likelihood
#   loglk<-0
#   # Iterate over countries
#   for (iso in 1:fdf$n_isos) {
#     # Save on computation
#     hs <- params$hsev[fdf$htype[iso, 1:fdf$n_dis[iso]]]
#     iproxhs <- fdf$iprox[iso, 1:fdf$n_dis[iso], ] / hs
#     if(fdf$n_dis[iso] == 1) iproxhs%<>%matrix(nrow=1)
#     lnmu_AR1 <- fdf$lnmu_AR1[iso, ]
#     lnsig_AR1 <- fdf$lnsig_AR1[iso, ]
#     # Iterate over time
#     for (ttt in 2:fdf$n_t) {
#       # Extract flag for disaster impact
#       flag_mat <- matrix(fdf$flag[iso, ttt, 1:fdf$n_dis[iso]],nrow=fdf$n_dis[iso],ncol=1)
#       # # Disaster severity calculation
#       if (sum(flag_mat) > 0) {
#         # calculate the severity
#         dsev <- t(iproxhs) %*% (flag_mat * (
#           hs * fdf$hazdur[iso, ttt, 1:fdf$n_dis[iso]] * params$beta_dur +
#             exp(-fdf$ts[iso, ttt, 1:fdf$n_dis[iso]] / hs) -
#             exp(-fdf$tf[iso, ttt, 1:fdf$n_dis[iso]] / hs)))
#       } else dsev <-rep(0,fdf$n_com)
#       # Compute GPR mean function
#       mu <- params$beta_dis * fdf$dsev[iso,ttt,] * params$isev +
#         params$beta_y1 * lnmu_AR1 * fdf$lny[iso, ttt-1, ]
#       # Compute adjusted sigma for AR1
#       red_sig <- lnsig_AR1 * params$sigma
#       # Log-likelihood
#       loglk<-loglk+sum(dnorm(fdf$lny[iso, ttt, ],
#                          mu,red_sig,log = T))
#     }
#   }
#   
#   return(loglk)
# }
# 
# # Calculate the non-log production value model likelihood
# m_likelihood_nology <- function(fdf, params) {
#   # Initialise log-likelihood
#   loglk<-0
#   # Iterate over countries
#   for (iso in 1:fdf$n_isos) {
#     # Save on computation
#     hs <- params$hsev[fdf$htype[iso, 1:fdf$n_dis[iso]]]
#     iproxhs <- fdf$iprox[iso, 1:fdf$n_dis[iso], ] / hs
#     if(fdf$n_dis[iso] == 1) iproxhs%<>%matrix(nrow=1)
#     mu_AR1 <- fdf$mu_AR1[iso, ]
#     sig_AR1 <- fdf$sig_AR1[iso, ]
#     # Iterate over time
#     for (ttt in 2:fdf$n_t) {
#       # Extract flag for disaster impact
#       flag_mat <- matrix(fdf$flag[iso, ttt, 1:fdf$n_dis[iso]],nrow=fdf$n_dis[iso],ncol=1)
#       # Disaster severity calculation
#       if (sum(flag_mat) > 0) {
#         # calculate the severity
#         dsev <- t(iproxhs) %*% (flag_mat * (
#           hs * fdf$hazdur[iso, ttt, 1:fdf$n_dis[iso]] * params$beta_dur +
#             exp(-fdf$ts[iso, ttt, 1:fdf$n_dis[iso]] / hs) -
#             exp(-fdf$tf[iso, ttt, 1:fdf$n_dis[iso]] / hs)))
#       } else dsev <-rep(0,fdf$n_com)
#       # Compute GPR mean function
#       mu <- params$beta_dis * dsev * params$isev +
#         params$beta_y1 * mu_AR1 * fdf$y[iso, ttt-1, ]
#       # Compute adjusted sigma for AR1
#       red_sig <- sig_AR1 * params$sigma
#       # Log-likelihood
#       loglk<-loglk+sum(dnorm(fdf$y[iso, ttt, ],
#                              mu,red_sig,log = T))
#     }
#   }
#   
#   return(loglk)
# }

# # Binary disaster occurrence variable likelihood
# m_likelihood_bin <- function(fdf, params) {
#   # Initialise log-likelihood
#   loglk<-0
#   # Iterate over countries
#   for (iso in 1:fdf$n_isos) {
#     # Save on computation
#     # hs <- params$hsev[fdf$htype[iso, 1:fdf$n_dis[iso]]]
#     # iproxhs <- fdf$iprox[iso, 1:fdf$n_dis[iso], ] / hs
#     # if(fdf$n_dis[iso] == 1) iproxhs%<>%matrix(nrow=1)
#     lnmu_AR1 <- fdf$lnmu_AR1[iso, ]
#     lnsig_AR1 <- fdf$lnsig_AR1[iso, ]
#     # Iterate over time
#     for (ttt in 2:fdf$n_t) {
#       # Extract flag for disaster impact
#       # flag_mat <- matrix(fdf$flag[iso, ttt, 1:fdf$n_dis[iso]],nrow=fdf$n_dis[iso],ncol=1)
#       # Disaster severity calculation
#       # if (sum(flag_mat) > 0) {
#         # calculate the severity
#       #   dsev <- t(iproxhs) %*% (flag_mat * (
#       #     hs * fdf$hazdur[iso, ttt, 1:fdf$n_dis[iso]] * params$beta_dur +
#       #       exp(-fdf$ts[iso, ttt, 1:fdf$n_dis[iso]] / hs) -
#       #       exp(-fdf$tf[iso, ttt, 1:fdf$n_dis[iso]] / hs)))
#       # } else dsev <-rep(0,fdf$n_com)
#       # Compute GPR mean function
#       mu <- params$beta_dis * as.integer(any(fdf$hazdur[iso, ttt, 1:fdf$n_dis[iso]]>0)) * params$isev +
#         params$beta_y1 * lnmu_AR1 * fdf$lny[iso, ttt-1, ]
#       # Compute adjusted sigma for AR1
#       red_sig <- lnsig_AR1 * params$sigma
#       # Log-likelihood
#       loglk<-loglk+sum(dnorm(sum(fdf$lny[iso, ttt, ]),
#                              sum(mu),mean(red_sig),log = T))
#     }
#   }
#   
#   return(loglk)
# }

# # Kalman filter likelihood model
# m_likelihood_KF <- function(fdf, params) {
#   # Initialise log-likelihood
#   loglk <- 0
#   # Iterate over countries
#   for (iso in 1:fdf$n_isos) {
#     # Save on computation
#     lnmu_AR1 <- fdf$lnmu_AR1[iso, ]
#     lnsig_AR1 <- fdf$lnsig_AR1[iso, ]
#     # Initial state estimate
#     x_hat <- fdf$lny[iso, 1, ]  # Initial state = first observed log price
#     P <- diag(lnsig_AR1^2)       # Initial covariance matrix
#     # Define noise terms
#     # Q <- diag(params$sigma^2)   # Process noise covariance (tuned parameter)
#     # R <- diag(lnsig_AR1^2)  # Observation noise
#     # Iterate over time
#     for (ttt in 2:fdf$n_t) {
#       # If there are disasters occurring during year ttt then calculate the disaster severity
#       if(sum(fdf$flag[iso, ttt, 1:fdf$n_dis[iso]])!=0 & sum(fdf$iprox[iso, 1:fdf$n_dis[iso], k])!=0){
#         # Calculate the disaster severity
#         dsev <- sapply(1:fdf$n_com,function(k){
#           log(10+sum(exp(fdf$iprox[iso, 1:fdf$n_dis[iso], k]) * fdf$flag[iso, ttt, 1:fdf$n_dis[iso]] * params$hsev[fdf$htype[iso, 1:fdf$n_dis[iso]]])) # * fdf$hazdur[iso, ttt, 1:fdf$n_dis[iso]]))
#         })
#       } else dsev <- rep(0,fdf$n_com)
#       # Compute GPR mean function
#       mu <- params$beta_dis * dsev * params$isev +
#         params$beta_y1 * lnmu_AR1 * fdf$lny[iso, ttt-1, ]
#       
#       # **Step 1: Prediction**
#       F <- diag(params$beta_y1 * lnmu_AR1)  # Transition matrix (AR1 process)
#       G <- diag(params$beta_dis * params$isev)  # Disaster severity impact
#       x_pred <- F %*% x_hat + G %*% dsev  # Predicted state
#       P_pred <- F %*% P %*% t(F) #+ Q  # Predicted covariance
#       
#       # **Step 2: Update**
#       # H <- diag(1, fdf$n_com)  # Observation matrix (identity)
#       y_obs <- fdf$lny[iso, ttt, ]  # Observed log price
#       # y_pred <- H %*% x_pred  # Predicted observation
#       y_pred<-x_pred
#       
#       # Kalman gain
#       # S <- H %*% P_pred %*% t(H) #+ R
#       S<-P_pred
#       
#       # Compute log-likelihood contribution
#       loglk <- loglk + sum(Rfast::dmvnorm(y_obs, mu = as.vector(y_pred), sigma = S, logged = TRUE))
#     }
#   }
#   
#   return(loglk)
# }

# Given a set of model parameters, calculate different elements of the likelihood
# generate_y <- function(fdf, params) {
#   # Initialize y (output commodity data)
#   outs <- data.frame()
#   # Iterate over countries
#   for (iso in 1:fdf$n_isos) {
#     mu <- numeric(fdf$n_com)  # GPR mean function
#     dsev <- numeric(fdf$n_com)  # Disaster severity per commodity
#     dsevhaz <- numeric(fdf$n_com)  # Disaster severity per commodity
#     dsevimp <- numeric(fdf$n_com)  # Disaster severity per commodity
#     # Iterate over time
#     for (ttt in 1:fdf$n_t) {
#       # Reset disaster severity each year
#       dsev[] <- dsevhaz[] <- dsevimp[] <- 0  
#       # Extract flag for disaster impact
#       flag_vec <- fdf$flag[iso, ttt, 1:fdf$n_dis[iso]]
#       # Disaster severity calculation
#       if (sum(flag_vec) > 0) {
#         for (ic in 1:fdf$n_com) {
#           hs <- params$hsev[fdf$htype[iso, 1:fdf$n_dis[iso]]]
#           iproxhs <- fdf$iprox[iso, 1:fdf$n_dis[iso], ic] / hs
#           # calculate the severity
#           dsev[ic] <- sum(flag_vec * iproxhs * (
#             hs * fdf$hazdur[iso, ttt, 1:fdf$n_dis[iso]] * params$beta_dur +
#               exp(-fdf$ts[iso, ttt, 1:fdf$n_dis[iso]] / hs) -
#                                     exp(-fdf$tf[iso, ttt, 1:fdf$n_dis[iso]] / hs)))
#           # Only the hazard part of the severity
#           dsevhaz[ic] <- sum(flag_vec * iproxhs * (
#             hs * fdf$hazdur[iso, ttt, 1:fdf$n_dis[iso]] * params$beta_dur))
#           # Only the impact decay part of the severity
#           dsevimp[ic] <- sum(flag_vec * iproxhs * (
#               exp(-fdf$ts[iso, ttt, 1:fdf$n_dis[iso]] / hs) -
#               exp(-fdf$tf[iso, ttt, 1:fdf$n_dis[iso]] / hs)))
#           # iprox_vec <- exp(fdf$iprox[iso, 1:fdf$n_dis[iso], ic])
#           # iphs <- iprox_vec / params$hsev[fdf$htype[iso, 1:fdf$n_dis[iso]]]
#           # # calculate the severity
#           # dsev[ic] <- sum(flag_vec * (
#           #     iphs * fdf$hazdur[iso, ttt, 1:fdf$n_dis[iso]] * params$beta_dur +
#           #       iprox_vec * iphs * (exp(-fdf$ts[iso, ttt, 1:fdf$n_dis[iso]] / iphs) -
#           #                             exp(-fdf$tf[iso, ttt, 1:fdf$n_dis[iso]] / iphs))))
#         }
#       }
#       # Compute GPR mean function
#       if (ttt == 1) {
#         mu <- log(10+fdf$y[iso, ttt, ])  # Use first-year observed data
#         dsev_mag <- ar_mag <- NA
#       } else {
#         mu <- params$beta_dis * dsev * params$isev +
#           params$beta_y1 * fdf$lnmu_AR1[iso, ] * log(10+fdf$y[iso, ttt-1, ])
#         dsev_mag <- params$beta_dis * dsev * params$isev
#         ar_mag <- params$beta_y1 * fdf$lnmu_AR1[iso, ] * log(10+fdf$y[iso, ttt-1, ])
#       }
#       
#       # Compute adjusted sigma for AR1
#       red_sig <- fdf$lnsig_AR1[iso, ] * params$sigma
#       
#       outs%<>%rbind(data.frame(time=ttt,com=1:fdf$n_com,ISO3=fdf$isos[iso],
#                                y=mu,
#                                ydiff=(log(10+fdf$y[iso, ttt, ]) - mu),
#                                sigma=red_sig,
#                                dsev=dsev_mag,
#                                dsevimp=dsevimp,
#                                dsevhaz=dsevhaz,
#                                ar=ar_mag))
#     }
#   }
#   
#   outs%>%mutate(yobs=ydiff+y)%>%return()
# }