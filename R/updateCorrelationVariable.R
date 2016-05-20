updateCorrelationVariable <- function(x, corData, mydata, countRow){
  i <- which(colnames(corData)==x)
  data <- as.numeric(mydata[,x])
  countNa <- sum(is.na(data))
  varRoundPercentage <- round((countNa/countRow*100), digits = 3)
  varRoundSd <-  round(sd(data, na.rm = T), digits = 3)
  varRoundDeterminant <- (det(corData[-i,-i]))

  dfRow <- c("correlationCutoffFilter", class(data),varRoundPercentage ,
           countNa, varRoundSd, NA, NA, varRoundDeterminant )

  return(cbind(t(dfRow)))
}

