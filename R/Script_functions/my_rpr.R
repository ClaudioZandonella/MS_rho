#---------------------------------------------------#
#----    Alterantive version of the function    ----#
#---------------------------------------------------#

library(MASS)

#----    ttor    ----

ttor<-function(tc,n){
  sqrt((tc/sqrt(n-2))^2/(1+(tc/sqrt(n-2))^2))
}

#----    compute_min_sign_r    ----

compute_min_sign_r <- function(alternative, sig_level, n, tcrit){

  if (alternative=="two.sided"){
    tcrit<-qt(1-sig_level/2,n-2)
    rtemp<-ttor(tcrit,n)
    min.sig.r<-c(-rtemp,rtemp)
  }else if (alternative=="greater"){
    tcrit<-qt(1-sig_level,n-2)
    rtemp<-ttor(tcrit,n)
    min.sig.r<-c(rtemp)
  }else if (alternative=="less"){
    tcrit<-qt(1-sig_level,n-2)
    rtemp<-ttor(tcrit,n)
    min.sig.r<-c(-rtemp)
  }

  return(min.sig.r)
}


#----    my_retro.r    ----

my_retro.r<-function(rho,n,sig_level=.05,B=10000,alternative = c("two.sided","less","greater"), seed=NULL)
{
  # Set seed
  if(!is.null(seed)){set.seed(seed = seed)}

  # Check on alternative
  alternative = match.arg(alternative)
  if(rho>0 && alternative=="less") stop("If rho > 0, alternative must be two.sided or greater")
  if(rho<0 && alternative=="greater") stop("If rho < 0, alternative must be two.sided or less")

  # Simulate values loop
  r_sim<-p_val<-vector(mode = "double", length = B)
  for(i in 1:B) {
    d<-mvrnorm(n=n,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),ncol=2))
    res_cor.test = cor.test(d[,1],d[,2],alternative = alternative)
    r_sim[i] = res_cor.test$estimate
    p_val[i] = res_cor.test$p.value
  }

  # Compute results
  power = sum( p_val < sig_level ) / B
  typeS=0
  if(alternative == "two.sided") {
    if (rho>0) {typeS <- sum( (p_val < sig_level) & (r_sim<0)) / sum(p_val < sig_level)}
    else {typeS <- sum( (p_val < sig_level) & (r_sim>0)) / sum(p_val < sig_level)}}
  typeM <- mean(abs(r_sim[p_val< sig_level])) / abs(rho) # cosi OK anche nel caso di less

  # minimum significant r
  min_sig_r = compute_min_sign_r(alternative = alternative, sig_level = sig_level,
                                 n = n, tcrit = tcrit)

  # Result object
  out<-list(rho=rho, sig_level= sig_level, alternative=alternative,
            power=power,typeM=typeM,typeS=typeS,min_sig_r=min_sig_r)

  return(out)
}


#----    my_pro.r    ----

my_pro.r<-function(rho,sig_level=.05,power=.80,rangen = c(1,1000), B = 1e4, tol = .005,
                    alternative = c("two.sided","less","greater"), seed = NULL)
{
  # Set seed
  if(!is.null(seed)){set.seed(seed = seed)}

  # Check on alternative
  alternative = match.arg(alternative)
  if(rho>0 && alternative=="less") stop("If rho > 0, alternative must be two.sided or greater")
  if(rho<0 && alternative=="greater") stop("If rho < 0, alternative must be two.sided or less")

  # Set values
  r_sim<-p_val<-vector(mode = "double", length = B)
  n_seq <- seq( rangen[1], rangen[2], by = 1 )
  (n_target <- round(median(n_seq)))
  find_power <- FALSE

  # Check with maximum N
  for (i in 1:B){
    d<-mvrnorm(n=rangen[2],mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),ncol=2))
    p_val[i] = cor.test(d[,1],d[,2],alternative = alternative)$p.value
  }
  est_power<-sum(p_val<sig_level)/B

  if ( est_power < power ) {
    cat(paste0("Actual power = ", est_power, " with n = ", rangen[2]),"\n")
    cat(paste0("   try to increase maximum of rangen > ", rangen[2],"."),"\n")
    out <- NULL
  } else {

    # Find the required sample size
    while( (!find_power) ) {

      # Simulate values loop
      for(i in 1:B) {
        d<-mvrnorm(n=n_target,mu=c(0,0),Sigma=matrix(c(1,rho,rho,1),ncol=2))
        res_cor.test = cor.test(d[,1],d[,2],alternative = alternative)
        r_sim[i] = res_cor.test$estimate
        p_val[i] = res_cor.test$p.value
      }

      est_power=sum(p_val<sig_level)/B

      # Evaluate if power was obtained according to tolerance value
      if ( (est_power<=(power+tol)) && (est_power>=(power-tol)) ) {
        find_power <- TRUE
      } else {
        if (length(n_seq)==1) { stop(" ")
        } else if ( est_power > (power-tol) ) {
          (n_seq <- seq( min(n_seq), n_target, by = 1))
          (n_target <- round(median(n_seq)))
        } else {
          (n_seq <- seq( n_target, max(n_seq), by = 1))
          (n_target <- round(median(n_seq)))
        }
      }
    }

    # Compute results
    typeS<-0
    if(alternative == "two.sided") {
      if (rho>0) {typeS <- sum( (p_val < sig_level) & (r_sim<0)) / sum(p_val < sig_level)}
      else {typeS <- sum( (p_val < sig_level) & (r_sim>0)) / sum(p_val < sig_level)}
    }
    typeM <- mean(abs(r_sim[p_val< sig_level])) / abs(rho)
    # mean sig r

    min_sig_r = compute_min_sign_r(alternative = alternative, sig_level = sig_level,
                                   n = n_target, tcrit = tcrit)

    # Result object
    out <- list( rho = rho, sig_level= sig_level, alternative=alternative,  power = est_power, n = n_target,
                 typeM = typeM, typeS=typeS, min_sig_r=min_sig_r)
  }
  if (!is.null(out))  return(out)
}
