###################################
####    Auxiliary functions    ####
###################################

# Include auxiliary functions used for plots

#----    sample_rho

sample_rho <- function(rho = 0, n = 30, B = 10000){
  rho_sim <- replicate(B, {
    data <-  mvrnorm(n, mu = c(0,0), Sigma = matrix(c(1,rho,rho,1), ncol = 2))
    cor(data[,1], data[,2])
   })

  return(rho_sim)
}