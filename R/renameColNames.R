renameColNames <- function(numberOfFactors){
  factorNames <- c()
  numberOfPlaces <- nchar(as.character(numberOfFactors))
  for (f in 1:numberOfFactors) {
    numberCol <- stringr::str_pad(as.character(f), width = numberOfPlaces, pad = "0")
    factorNames=c(factorNames, paste("Component", numberCol, sep=""))
  }
  return(factorNames)
}
