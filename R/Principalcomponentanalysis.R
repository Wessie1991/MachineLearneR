Principalcomponentanalysis <- function(mydata, selectedNumberOfDimensions, covList, faMethodScores){
  solution <-PCA(mydata = mydata,
                   selectedNumberOfDimensions = selectedNumberOfDimensions, covList=covList)
  solution$loadings <- as.matrix(solution$loadings[,order(colnames(solution$loadings))])

  factorz <- robustfa::computeScores(solution, x = mydata,
                                     covmat = covList, cor = TRUE, scoresMethod = faMethodScores)
  solution$mydata <- data.frame(factorz$scores)
  return(solution)
}
