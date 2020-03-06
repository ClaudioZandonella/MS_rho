##### GENERAL INFORMATION
###### rpr1 aggiornata:
###### 1) prospective e retrospective
###### 2) come alternativa: two.sided, less e greater
###### 3) aggiunti alcuni controlli
###### 4) piccole modifiche all'output (ho inserito min.sig.r e tolto mean.r.sig)
#
library(MASS)
#
## retro.r
retro.r<-function(rho,n,sig.level=.05,B=10000,alternative = c("two.sided","less","greater"))
{
  alternative = match.arg(alternative)
  # check on alternative
  if(rho>0 & alternative=="less") stop("If rho > 0, alternative must be two.sided or greater")
  if(rho<0 & alternative=="greater") stop("If rho < 0, alternative must be two.sided or less")
  #
  r.sim<-pval<-d<-NULL
  for(i in 1:B) {
    d[[i]]<-mvrnorm(n=n,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),ncol=2))
    r.sim[i]<-cor(d[[i]])[1,2]
    pval[i]<-cor.test(d[[i]][,1],d[[i]][,2],alternative = alternative)$p.value
  }
  power <- sum( pval < sig.level ) / B
  typeS=0
  if(alternative == "two.sided") {
    if (rho>0) {typeS <- sum( (pval < sig.level) & (r.sim<0)) / sum(pval < sig.level)}
    else {typeS <- sum( (pval < sig.level) & (r.sim>0)) / sum(pval < sig.level)}}
typeM <- mean(abs(r.sim[pval< sig.level])) / abs(rho) # cosi OK anche nel caso di less
# mean sig r
ttor<-function(tc,n) sqrt((tc/sqrt(n-2))^2/(1+(tc/sqrt(n-2))^2))

  if (alternative=="two.sided"){  
    tcrit<-qt(1-sig.level/2,n-2)
    rtemp<-ttor(tcrit,n)
    min.sig.r<-c(-rtemp,rtemp)
  }
  if (alternative=="greater"){  
    tcrit<-qt(1-sig.level,n-2)
    rtemp<-ttor(tcrit,n)
    min.sig.r<-c(rtemp)
  }
  if (alternative=="less"){  
    tcrit<-qt(1-sig.level,n-2)
    rtemp<-ttor(tcrit,n)
    min.sig.r<-c(-rtemp)
  }
#
  out<-list(rho=rho, sig.level= sig.level, alternative=alternative,power=power,typeM=typeM,typeS=typeS,min.sig.r=min.sig.r)
  return(out)
}
#
## Examples:
### retro.r(.20,n=40)
### retro.r(-.20,n=40)
### retro.r(.20,n=40,alternative = "greater")
### retro.r(-.20,n=40,alternative = "less")
### Miss-specification:
#### retro.r(.20,n=40,alternative = "less")
#### retro.r(-.20,n=40,alternative = "greater")
#
pro.r<-function(rho,sig.level=.05,power=.80,rangen = c(1,1000), B = 1e4, tol = .005,
                alternative = c("two.sided","less","greater"))
{
  alternative = match.arg(alternative)
  # check on alternative
  if(rho>0 & alternative=="less") stop("If rho > 0, alternative must be two.sided or greater")
  if(rho<0 & alternative=="greater") stop("If rho < 0, alternative must be two.sided or less")
  #
  r.sim<-pval<-NULL
  n_seq <- seq( rangen[1], rangen[2], by = 1 )
  (n_target <- round(median(n_seq)))
  find_power <- FALSE
  # check with maximum N
  for (i in 1:B){
    temp<-mvrnorm(n=rangen[2],mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),ncol=2))
    temp1<-cor.test(temp[,1],temp[,2],alternative = alternative)
    pval[i]<-temp1$p.value
  }
  est_power<-sum(pval<sig.level)/B
  if ( est_power < power ) {
    cat(paste0("Actual power = ", est_power, " with n = ", rangen[2]),"\n")
    cat(paste0("   try to increase maximum of rangen > ", rangen[2],"."),"\n")
    out <- NULL
  } else {## estimating power
    while( (!find_power) ) {
      for (i in 1:B){
        temp<-mvrnorm(n=n_target,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),ncol=2))
        temp1<-cor.test(temp[,1],temp[,2],alternative = alternative)
        r.sim[i]<-temp1$estimate ; pval[i]=temp1$p.value
      }
      est_power=sum(pval<sig.level)/B
      typeS<-0
      if(alternative == "two.sided") {
        if (rho>0) {typeS <- sum( (pval < sig.level) & (r.sim<0)) / sum(pval < sig.level)}
        else {typeS <- sum( (pval < sig.level) & (r.sim>0)) / sum(pval < sig.level)}
      }
      typeM <- mean(abs(r.sim[pval< sig.level])) / abs(rho)
      # mean sig r
      ttor<-function(tc,n) sqrt((tc/sqrt(n-2))^2/(1+(tc/sqrt(n-2))^2))
      
      if (alternative=="two.sided"){  
        tcrit<-qt(1-sig.level/2,n_target-2)
        rtemp<-ttor(tcrit,n_target)
        min.sig.r<-c(-rtemp,rtemp)
      }
      if (alternative=="greater"){  
        tcrit<-qt(1-sig.level,n_target-2)
        rtemp<-ttor(tcrit,n_target)
        min.sig.r<-c(rtemp)
      }
      if (alternative=="less"){  
        tcrit<-qt(1-sig.level,n_target-2)
        rtemp<-ttor(tcrit,n_target)
        min.sig.r<-c(-rtemp)
      }
      #
      
      if ( (est_power<=(power+tol)) & (est_power>(power-tol)) ) {
        find_power <- TRUE
      } else {
        if (length(n_seq)==1) {stop(" ")
        }
        if ( est_power > (power-tol) ) {
          (n_seq <- seq( min(n_seq), n_target, by = 1))
          (n_target <- round(median(n_seq)))
        } else {
          (n_seq <- seq( n_target, max(n_seq), by = 1))
          (n_target <- round(median(n_seq)))
        }
      }
    }
    out <- list( rho = rho, sig.level= sig.level, alternative=alternative,  power = power, n = n_target,
                 typeS=typeS,typeM = typeM,min.sig.r=min.sig.r)
  }
  if (!is.null(out))  return(out)
}
#
## Examples:
### pro.r(rho=.30,sig.level = .05, power=.80)
### check interno retro.r(rho=.30,n=84) ## OK!
### pro.r(rho=-.30,sig.level = .05, power=.80)
### pro.r(rho=.30,sig.level = .05, power=.80,alternative="greater")
### pro.r(rho=-.30,sig.level = .05, power=.80,alternative="less")
### Miss-specification:
### pro.r(rho=.30,sig.level = .05, power=.80,alternative="less")
### pro.r(rho=-.30,sig.level = .05, power=.80,alternative="greater")
###
###
# g.b. & g.a, 2020