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
  if(empAR) return(function(chainnum){
    list(beta_y1 = 1,
         sigma = 1)
  })
  if(!GPR) {
    if(iprox_dat){
      return(function(chainnum){
        list(iprox = fdf$mu_dis,
             beta_y1 = fdf$mu_AR1,
             sigma = fdf$sig_AR1)
      })
    } else {
      return(function(chainnum){
        list(beta_y1 = fdf$mu_AR1,
             sigma = fdf$sig_AR1)
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
           rho = rho_est,
           alpha = alpha_est,
           beta_y1 = fdf$mu_AR1)
    })
  } else {
    return(function(chainnum){
      list(rho = rho_est,
           alpha = alpha_est,
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