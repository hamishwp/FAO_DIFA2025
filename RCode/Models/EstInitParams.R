InitParams<-function(fdf){
  
  # From GPR regression directly on production data
  rho
  alpha
  # For iprox, mu_dis and sig_dis we should max-normalise them to be less than the countries maximum production over 1991-present
  
  return(function(chainnum){
    list(
      iprox = fdf$mu_dis,
      rho = 
      alpha = 
      beta_y1 = fdf$mu_AR1
    )
  })
}