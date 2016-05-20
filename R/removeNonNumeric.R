removeNonNumeric=function(x) {
  class=class(x)
  if (class!="numeric" & class!="integer" ) {
    return(c(class, (sum(is.na(x))/length(x)*100), sum(is.na(x)) ,"removeNonNumeric"))
  }
}
