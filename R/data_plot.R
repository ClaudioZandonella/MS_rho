##############################
####    Data for Plots    ####
##############################


#----    Settings     ----
library(tidyverse)
library(future.apply)
library(tictoc)

source("R/Design_analysis_r.R")
source("R/Auxiliary_functions.R")

plan(multiprocess, workers = 4)

load("Data/data_conditions.rda")
load("Data/data_hypothesis.rda")

#----    Data_plot_scenarios    ----

conditions <- expand.grid(rho = c(.25, .50, .75),
                          n = c(3:20,seq(from=22, to=40, by=2),seq(from=45, to=100, by=5), seq(from=110, to=200, by=10),250,300,500))
conditions$B <- c(rep(7e5, 9),rep(5e4, 150))


data_conditions <- future_mapply(retro_r, conditions$rho,conditions$n, B = conditions$B, seed=2020)

data_conditions <- data_conditions%>%
  t()%>%
  as.data.frame()%>%
  mutate_at(vars("rho":"typeS"),unlist)

save(data_conditions, file="Data/data_conditions.rda")

#----    Data_plot_hypothesis    ----

load("Data/data_conditions.rda")

n_list <- data.frame(n = c(3:20,seq(from=22, to=40, by=2),seq(from=45, to=100, by=5), seq(from=110, to=200, by=10),250,300,500))
n_list$B <- c(rep(7e5, 3),rep(5e4, 50))

data_hypothesis <- future_mapply(retro_r, rho = .25, n = n_list$n,
                                 alternative ="greater", B = n_list$B, seed=2020)

data_hypothesis <- data_hypothesis%>%
  t()%>%
  as.data.frame()%>%
  mutate_at(vars("rho":"typeS"),unlist)

data_hypothesis<- data_conditions %>%
  filter(rho == .25)%>%
  rbind(data_hypothesis)

save(data_hypothesis, file="Data/data_hypothesis.rda")

#----    Data_plot_cohen    ----

conditions_cohen <- expand.grid(d = c(.2, .5, .7, .9),
                                n = c(seq(1,25, by=.2), 25:50,seq(55,100, by=5)))

data_cohen <- future_mapply(retrodesignD, conditions_cohen$d, n = conditions_cohen$n,
                            B = 2e5, seed=2020)

data_cohen <- data_cohen%>%
  t()%>%
  as.data.frame()%>%
  mutate_at(vars("d":"typeM"),unlist)

save(data_cohen, file = "data/data_cohen.rda")

#----    Data_plot_rho    ----

conditions_rho <- expand.grid(rho = c(.2, .5, .7, .9),
                              n = c(3:20,seq(from=22, to=40, by=2),seq(from=45, to=100, by=5), seq(from=110, to=200, by=10),250,300,500))
conditions_rho$B <- c(rep(7e5, 12),rep(5e4, 200))

data_conditions_rho <- future_mapply(retro_r, conditions_rho$rho[1:12],conditions_rho$n[1:12],
                                     B = conditions_rho$B[1:12], seed=2020)

data_conditions_rho <- data_conditions_rho%>%
  t()%>%
  as.data.frame()%>%
  mutate_at(vars("rho":"typeS"),unlist)

save(data_conditions_rho, file="Data/data_conditions_rho.rda")

#----

