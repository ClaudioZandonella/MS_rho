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

#---------

x <- rnorm(30)
y <- rnorm(30)

cor.test(x,y)

obs_cor <- cor(x,y)
t_obs = obs_cor*sqrt((length(x)-2)/(1-obs_cor^2))
p_val = 2*pt(t_obs,(length(x)-2),lower.tail = FALSE)
ci_upper = tanh(atanh(obs_cor)+qnorm(.975)/sqrt(length(x)-3))

round(p_val,4)
#-----

get_rho <- function(rho = 0, N = 30){

  data <- mvrnorm(n=N,mu = c(0,0), Sigma = matrix(c(1,rho,rho,1), ncol = 2))
  res <- cor(data[,1], data[,2])

  return(res)
}

sample_rho <- function(rho = 0, N = 30, B = 10000){
  rho_sim <- replicate(B, get_rho(rho, N))

  return(rho_sim)
}

data <- data.frame(rho = factor(rep(c(0,.4,.6,.8), each=10000)),
                   value = c(sample_rho(rho=0),
                             sample_rho(rho=.4),
                             sample_rho(rho=.6),
                             sample_rho(rho=.8)))

ggplot(data)+
  geom_density(aes(x=value, linetype=rho, col=rho))

#-----

res <- sample_rho(rho=.8, N = 20, B=5000)

tanh(mean(atanh(res)))
mean(res)


plot(density(data[data$rho==0,2]))
curve(dt(x,df=30-2), add = T)
curve((1-x^2)^((30-4)/2)/beta(1/2,(30-2)/2), add = T, col="red")



#-----    grafico    -----

library(tidyverse)


list2data <- function(list, transpose=TRUE, select=NULL){
  if(transpose) list <- t(list)

  if(!is.null(select)){
    slected_arg = dimnames(list)[[2]] %in% select

    save_names = dimnames(list)[[2]][slected_arg]
    save_dim = dim(list)
    save_dim[2] = length(save_names)

    list <- list[rep(slected_arg, each=dim(list)[1])]
    dim(list) <- save_dim
    dimnames(list) <- list(NULL,save_names)
  }

  data <- as.data.frame(matrix(unlist(list),ncol=dim(list)[2], dimnames = dimnames(list)))

  return(data)
}

retrodesign <- function(A, s, alpha=.05, df=Inf, n.sims=10000){
  z <- qt(1-alpha/2, df)
  p.hi <- 1 - pt(z-A/s, df)
  p.lo <- pt(-z-A/s, df)
  power <- p.hi + p.lo
  typeS <- p.lo/power
  estimate <- A + s*rt(n.sims,df)
  significant <- abs(estimate) > s*z
  exaggeration <- mean(abs(estimate)[significant])/A
  return(list(power=power, typeS=typeS, exaggeration=exaggeration))
}

#---- Gelman and Carlin
D_range <- c(seq(0,1,.01),seq(1,10,.1),100)

data_plotD <- sapply(D_range, retrodesign, s=1)%>%
  list2data()


ggplot(data_plotD)+
  geom_line(aes(power, typeS))

ggplot(data_plotD)+
  geom_line(aes(power, exaggeration))

#----
retrodesignD <- function(A, N, alpha=.05, df=Inf, n.sims=10000){
  s <- 1/sqrt(N)
  z <- qt(1-alpha/2, df)
  p.hi <- 1 - pt(z-A/s, df)
  p.lo <- pt(-z-A/s, df)
  power <- p.hi + p.lo
  typeS <- p.lo/power
  estimate <- A + s*rt(n.sims,df)
  significant <- abs(estimate) > s*z
  exaggeration <- mean(abs(estimate)[significant])/A
  return(list(power=power, typeS=typeS, exaggeration=exaggeration))
}

n_range_D <- c(seq(1,25, by=.2), 25:50,seq(55,100, by=5))

plot_datad_2 <- sapply(n_range_D, retrodesignD, A=.2, n.sims=2e5)%>%
  list2data()%>%
  mutate(d=.2,
         n=n_range_D)

plot_datad_5 <- sapply(n_range_D, retrodesignD, A=.5, n.sims=2e5)%>%
  list2data()%>%
  mutate(d=.5,
         n=n_range_D)

plot_datad_7 <- sapply(n_range_D, retrodesignD, A=.7, n.sims=2e5)%>%
  list2data()%>%
  mutate(d=.7,
         n=n_range_D)

plot_datad_9 <- sapply(n_range_D, retrodesignD, A=.9, n.sims=2e5)%>%
  list2data()%>%
  mutate(d=.9,
         n=n_range_D)

data_d <- rbind(plot_datad_2,plot_datad_5,plot_datad_7,plot_datad_9)

ggplot(data_d)+
  geom_line(aes(power, typeS, color=factor(d)))

ggplot(data_d)+
  geom_line(aes(power, exaggeration, color=factor(d)))


#---- Correlation

retrodesign_r <- function(r, n, alpha=.05, n.sims=10000){
  fisher_r = atanh(r)
  fisher_se = 1/sqrt(n-3)
  r_sim = tanh(rnorm(n.sims, fisher_r, fisher_se))

  t_crit = qt(1-alpha/2,df = n-2)
  r_crit = t_crit/sqrt(n-2+t_crit^2)

  power = mean(abs(r_sim)>=r_crit)
  typeS = mean(r_sim<=-r_crit)/power
  typeM = mean(r_sim[abs(r_sim)>=r_crit])/r

  mean(r_sim>=r_crit)

  return(list(power=power, typeS=typeS, typeM=typeM))
}


plot_dataR <- sapply(r_range, retrodesign_r, n=20)%>%
  list2data()%>%
  mutate(rho=r_range)

ggplot(plot_dataR)+
  geom_line(aes(power, typeS))

ggplot(plot_dataR)+
  geom_line(aes(power, exaggeration))



n_range <- c(seq(4,25, by=.2), 25:50,seq(55,100, by=5))

n_range_bis <- c(seq(8.4,25, by=.3), 25:50,seq(55,100, by=5),seq(110,200, by=10),500)
plot_dataR_2 <- sapply(n_range_bis, retrodesign_r, r=.2, n.sims=5e6)%>%
  list2data()%>%
  mutate(r=.2,
         n=n_range_bis)

plot_dataR_5 <- sapply(n_range[-(1:5)], retrodesign_r, r=.5, n.sims=5e5)%>%
  list2data()%>%
  mutate(r=.5,
         n=n_range[-(1:5)])

plot_dataR_7 <- sapply(n_range[-(1:2)], retrodesign_r, r=.7, n.sims=5e5)%>%
  list2data()%>%
  mutate(r=.7,
         n=n_range[-(1:2)])

plot_dataR_9 <- sapply(n_range, retrodesign_r, r=.9, n.sims=5e5)%>%
  list2data()%>%
  mutate(r=.9,
         n=n_range)

data_R <- rbind(plot_dataR_2,plot_dataR_5,plot_dataR_7,plot_dataR_9)

ggplot(data_R)+
  geom_line(aes(power, typeS, color=factor(r)))

ggplot(data_R)+
  geom_line(aes(power, typeM, color=factor(r)))

#-----

data_d$effect <- "d"
names(data_d)[4] <- "value"
names(data_d)[3] <- "typeM"
data_R$effect <- "rho"
names(data_R)[4] <- "value"

rbind(data_d, data_R)%>%
  pivot_longer(c("typeS","typeM"), names_to = "error",values_to = "error_val")%>%
  filter(effect=="d")%>%
  ggplot()+
  geom_line(aes(power, error_val, color=factor(value)))+
  facet_wrap(factor(effect)~error, scales="free_y")

rbind(data_d, data_R)%>%
  pivot_longer(c("typeS","typeM"), names_to = "error",values_to = "error_val")%>%
  filter(effect=="")%>%
  ggplot()+
  geom_line(aes(power, error_val, color=factor(value)))+
  facet_wrap(.~error, scales="free_y")

#-----

data_R[c(1,9),]

n_range_prova=c(5:20,seq(from=22, to=40, by=2),seq(from=45, to=100, by=5),
    seq(from=110, to=200, by=10),250,300,500)


prova_2 <- sapply(n_range_prova,FUN=my_retro.r, rho=.2, B=5e4)%>%
  list2data(select = c("rho","power","typeM","typeS"))%>%
  mutate(n=n_range_prova)

prova_5 <- sapply(n_range_prova,FUN=my_retro.r, rho=.5, B=5e4)%>%
  list2data(select = c("rho","power","typeM","typeS"))%>%
  mutate(n=n_range_prova)

prova_7 <- sapply(n_range_prova,FUN=my_retro.r, rho=.7, B=5e4)%>%
  list2data(select = c("rho","power","typeM","typeS"))%>%
  mutate(n=n_range_prova)

prova_9 <- sapply(n_range_prova,FUN=my_retro.r, rho=.9, B=5e4)%>%
  list2data(select = c("rho","power","typeM","typeS"))%>%
  mutate(n=n_range_prova)


data_prova_plot <- rbind(prova_2,prova_5,prova_7,prova_9)


data_rho <- data_prova_plot

ggplot(data_prova_plot)+
  geom_line(aes(power, typeS, col=factor(rho)))

ggplot(data_prova_plot)+
  geom_line(aes(power, typeM, col=factor(rho)))

t(k_res)

add_n <- 3:4
add_2 <- sapply(add_n, my_retro.r, rho=.2, B=7e5)%>%
  list2data(select = c("rho","power","typeM","typeS"))%>%
  mutate(n=add_n)
add_5 <- sapply(add_n, my_retro.r, rho=.5, B=7e5)%>%
  list2data(select = c("rho","power","typeM","typeS"))%>%
  mutate(n=add_n)
add_7 <- sapply(add_n, my_retro.r, rho=.7, B=7e5)%>%
  list2data(select = c("rho","power","typeM","typeS"))%>%
  mutate(n=add_n)
add_9 <- sapply(add_n, my_retro.r, rho=.9, B=7e5)%>%
  list2data(select = c("rho","power","typeM","typeS"))%>%
  mutate(n=add_n)

prova_2 <- rbind(add_2,prova_2)
prova_5 <- rbind(add_5,prova_5)
prova_7 <- rbind(add_7,prova_7)
prova_9 <- rbind(add_9,prova_9)


my_retro.r(rho = .7, n = 3, B=1e5)
my_retro.r(rho = .5, n = 8, B=5e5)

1.4*.7

#------


#-------


#-----
