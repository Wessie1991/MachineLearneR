removeCategorical=function(x) {
  if (length(unique(x))<5) {
    return(c(length(unique(x)), (sum(is.na(x))/length(x)*100), sum(is.na(x)), "removeCategorical"))
  }
}
