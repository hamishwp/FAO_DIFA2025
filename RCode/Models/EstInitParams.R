# Given a set of model parameters, calculate different elements of the likelihood
generate_y <- function(fdf, params) {
  # Initialize y (output commodity data)
  outs <- data.frame()
  # Iterate over countries
  for (iso in 1:fdf$n_isos) {
    mu <- numeric(fdf$n_com)  # GPR mean function
    dsev <- numeric(fdf$n_com)  # Disaster severity per commodity
    dsevhaz <- numeric(fdf$n_com)  # Disaster severity per commodity
    dsevimp <- numeric(fdf$n_com)  # Disaster severity per commodity
    # Iterate over time
    for (ttt in 1:fdf$n_t) {
      # Reset disaster severity each year
      dsev[] <- dsevhaz[] <- dsevimp[] <- 0  
      # Extract flag for disaster impact
      flag_vec <- fdf$flag[iso, ttt, 1:fdf$n_dis[iso]]
      # Disaster severity calculation
      if (sum(flag_vec) > 0) {
        for (ic in 1:fdf$n_com) {
          hs <- params$hsev[fdf$htype[iso, 1:fdf$n_dis[iso]]]
          iproxhs <- fdf$iprox[iso, 1:fdf$n_dis[iso], ic] / hs
          # calculate the severity
          dsev[ic] <- sum(flag_vec * iproxhs * (
            hs * fdf$hazdur[iso, ttt, 1:fdf$n_dis[iso]] * params$beta_dur +
              exp(-fdf$ts[iso, ttt, 1:fdf$n_dis[iso]] / hs) -
                                    exp(-fdf$tf[iso, ttt, 1:fdf$n_dis[iso]] / hs)))
          # Only the hazard part of the severity
          dsevhaz[ic] <- sum(flag_vec * iproxhs * (
            hs * fdf$hazdur[iso, ttt, 1:fdf$n_dis[iso]] * params$beta_dur))
          # Only the impact decay part of the severity
          dsevimp[ic] <- sum(flag_vec * iproxhs * (
              exp(-fdf$ts[iso, ttt, 1:fdf$n_dis[iso]] / hs) -
              exp(-fdf$tf[iso, ttt, 1:fdf$n_dis[iso]] / hs)))
          # iprox_vec <- exp(fdf$iprox[iso, 1:fdf$n_dis[iso], ic])
          # iphs <- iprox_vec / params$hsev[fdf$htype[iso, 1:fdf$n_dis[iso]]]
          # # calculate the severity
          # dsev[ic] <- sum(flag_vec * (
          #     iphs * fdf$hazdur[iso, ttt, 1:fdf$n_dis[iso]] * params$beta_dur +
          #       iprox_vec * iphs * (exp(-fdf$ts[iso, ttt, 1:fdf$n_dis[iso]] / iphs) -
          #                             exp(-fdf$tf[iso, ttt, 1:fdf$n_dis[iso]] / iphs))))
        }
      }
      # Compute GPR mean function
      if (ttt == 1) {
        mu <- log(10+fdf$y[iso, ttt, ])  # Use first-year observed data
        dsev_mag <- ar_mag <- NA
      } else {
        mu <- params$beta_dis * dsev * params$isev +
          params$beta_y1 * fdf$lnmu_AR1[iso, ] * log(10+fdf$y[iso, ttt-1, ])
        dsev_mag <- params$beta_dis * dsev * params$isev
        ar_mag <- params$beta_y1 * fdf$lnmu_AR1[iso, ] * log(10+fdf$y[iso, ttt-1, ])
      }
      
      # Compute adjusted sigma for AR1
      red_sig <- fdf$lnsig_AR1[iso, ] * params$sigma
      
      outs%<>%rbind(data.frame(time=ttt,com=1:fdf$n_com,ISO3=fdf$isos[iso],
                               y=mu,
                               ydiff=(log(10+fdf$y[iso, ttt, ]) - mu),
                               sigma=red_sig,
                               dsev=dsev_mag,
                               dsevimp=dsevimp,
                               dsevhaz=dsevhaz,
                               ar=ar_mag))
    }
  }
  
  outs%>%mutate(yobs=ydiff+y)%>%return()
}

# params <- list(beta_dur = 1,
#                beta_dis = -1e-1,
#                hsev=rep(1,fdf$n_haz),
#                isev=rep(1,fdf$n_com),
#                beta_y1=rep(1,fdf$n_com),
#                sigma=1)
# outs<-generate_y(fdf,params)
# 
# outs%>%group_by(ISO3,com)%>%reframe(meandiff=mean(abs(ydiff),na.rm=T))%>%
#   ggplot()+geom_density(aes(abs(meandiff),colour=as.character(com)))+scale_x_log10()
# 
# range(outs$dsevhaz)
# range(outs$dsevimp)
# outs%>%
#   ggplot()+geom_point(aes(dsevhaz+1,dsevimp+1,colour=as.character(com)))+
#   scale_x_log10()+facet_wrap(~com)
# 
# outs%>%group_by(ISO3,com)%>%reframe(dsev=mean(dsev,na.rm=T),ar=mean(ar,na.rm=T))%>%group_by(com)%>%reframe(dsev=mean(dsev,na.rm=T),ar=mean(ar,na.rm=T))
# 
# outs%>%group_by(ISO3,com)%>%reframe(meandiff=mean(dsev-ar,na.rm=T))%>%
#   ggplot()+geom_density(aes(abs(meandiff),colour=as.character(com)))+scale_x_log10()
# 
# outs%>%ggplot()+geom_point(aes(y,sigma))+scale_x_log10()+scale_y_log10()+facet_wrap(~com)
# outs%>%ggplot()+geom_point(aes(y,yobs))+scale_x_log10()+scale_y_log10()+geom_abline(slope=1,intercept=0,colour="red")+
#   facet_wrap(~com)

# Calculate the model likelihood
m_likelihood <- function(fdf, params) {
  # Initialise log-likelihood
  loglk<-0
  # Iterate over countries
  for (iso in 1:fdf$n_isos) {
    # Iterate over time
    for (ttt in 1:fdf$n_t) {
      # Extract flag for disaster impact
      flag_vec <- fdf$flag[iso, ttt, 1:fdf$n_dis[iso]]
      # Disaster severity calculation
      if (sum(flag_vec) > 0) {
        hs <- params$hsev[fdf$htype[iso, 1:fdf$n_dis[iso]]]
        # calculate the severity
        dsev <- apply(flag_vec * fdf$iprox[iso, 1:fdf$n_dis[iso], ] / hs * (
          hs * fdf$hazdur[iso, ttt, 1:fdf$n_dis[iso]] * params$beta_dur +
            exp(-fdf$ts[iso, ttt, 1:fdf$n_dis[iso]] / hs) -
            exp(-fdf$tf[iso, ttt, 1:fdf$n_dis[iso]] / hs)),2,sum)
      } else dsev <-rep(0,fdf$n_com)
      # Compute GPR mean function
      mu <- params$beta_dis * dsev * params$isev +
        params$beta_y1 * fdf$lnmu_AR1[iso, ] * log(10+fdf$y[iso, ttt-1, ])
      # Compute adjusted sigma for AR1
      red_sig <- fdf$lnsig_AR1[iso, ] * params$sigma
      # Log-likelihood
      loglk<-loglk+pnorm(log(10+fdf$y[iso, ttt, ]),
                         mu,red_sig,log.p = T)
    }
  }
  
  return(loglk)
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
         gamAR1 = 1,
         sdAR1 = 1)
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