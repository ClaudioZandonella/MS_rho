##########################
####    Benchmarck    ####
##########################

library("tictoc")
library("profvis")

#----    load functions    ----#

source("R/Script_functions/rpr2.R")
source("R/Script_functions/my_rpr.R")


#----    profiling retro    -----
profvis::profvis(retro.r(.20,n=40, seed = 2020))
profvis::profvis(my_retro.r(.20,n=40, seed = 2020))


#----    bench retro    ----

banch_retro <- function(){
  tic()
  retro.r(.20,n=40, seed = 2020)
  retro.r(-.20,n=40, seed = 2020)
  retro.r(.20,n=40,alternative = "greater", seed = 2020)
  retro.r(-.20,n=40,alternative = "less", seed = 2020)
  retro.r(rho=.30,n=84, seed = 2020)
  toc()
}
replicate(10,banch_retro())
# 10.974 sec -  11.784

banch_my_retro <- function(){
  tic()
  my_retro.r(.20,n=40, seed = 2020)
  my_retro.r(-.20,n=40, seed = 2020)
  my_retro.r(.20,n=40,alternative = "greater", seed = 2020)
  my_retro.r(-.20,n=40,alternative = "less", seed = 2020)
  my_retro.r(rho=.30,n=84, seed = 2020)
  toc()
}
replicate(10,banch_my_retro())
# 9.637 sec - 10.503 sec



#----    profiling pro    -----
profvis::profvis(pro.r(rho=.30, power=.80, seed=2020))
profvis::profvis(my_pro.r(rho=.30, power=.80, seed=2020))


#----    bench pro    ----

banch_pro <- function(){
  tic()
  pro.r(rho=.30, power=.80, seed=2020)
  pro.r(rho=-.30, power=.80, seed=2020)
  pro.r(rho=.30, power=.80,alternative="greater", seed=2020)
  pro.r(rho=-.30, power=.80,alternative="less", seed=2020)
  toc()
}
replicate(5,banch_pro())
# 89.233 sec - 99.896 sec

banch_my_pro <- function(){
  tic()
  my_pro.r(rho=.30, power=.80, seed=2020)
  my_pro.r(rho=-.30, power=.80, seed=2020)
  my_pro.r(rho=.30, power=.80,alternative="greater", seed=2020)
  my_pro.r(rho=-.30, power=.80,alternative="less", seed=2020)
  toc()
}
replicate(5,banch_my_pro())
# 86.625 sec - 87.696 sec



#----
