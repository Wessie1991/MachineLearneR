removeMissing <- function(x, selectedGarbageCutoff){
  stdev <- sd(x, na.rm=TRUE)
  fractionMissing <- sum(is.na(x))/length(x)
  if(is.na(stdev)) {
    stdev <- 0
  }
  if (fractionMissing>=selectedGarbageCutoff ) {
    return(c(fractionMissing, (fractionMissing*length(x)*100), stdev, "removeMissing"))
  }else{
    if(stdev==0 | is.na(stdev)){
      return(c(fractionMissing, (fractionMissing*length(x)*100), stdev, "removeMissing"))
    }
  }
}
