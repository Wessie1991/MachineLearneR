# RobustZscore standardisation.
robustZscore=function (x) {
  meanAD=function (x) {
    mean=mean(x, na.rm=TRUE)
    y=mean(abs((x-mean)), na.rm=TRUE)
    return(y)
  }

  med <- median(x,na.rm=TRUE)
  nas<-sum(!is.na(x))
  if (nas !=0){
    mad = mad(x, na.rm=TRUE)
  }
  else {
    mad=0
  }

  meanADscore <- meanAD(x)

  if (mad==0) {
    y <- (x-med)/(1.253314*meanADscore)
  } else {
    y <- (x-med)/(1.4826*mad)
  }
  return(as.matrix(y))
}
