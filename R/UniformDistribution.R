uniformdistribution <- function(x){

  average=mean(x, na.rm = T)
  stdev=sd(x, na.rm = T)
  minv=min(x, na.rm = T)
  maxv=max(x, na.rm = T )

  # probability normal
  pnorm<-try(dnorm(x, average, stdev))
  pnorm<-try(sum(log(pnorm), na.rm = T))
  # probability uniform
  punif<-try(dunif(x, minv, maxv))
  punif<-try(sum(log(punif), na.rm = T))
  uniFormDistribution=try(punif-pnorm)
  if (uniFormDistribution > 0){
    return(c(uniFormDistribution,(sum(is.na(x))/length(x)*100), sum(is.na(x)), stdev, "uniFormDistribution"))
  }
}



