PCA <- function(mydata, selectedNumberOfDimensions, covList){

  solution <- psych::principal(r=cor(mydata), nfactors=selectedNumberOfDimensions,
                               rotate="varimax")
  solution$loadings <- as.matrix(solution$loadings[,order(colnames(solution$loadings))])
  factorz <- robustfa::computeScores(solution, x = mydata,
                                     covmat = covList, cor = TRUE, scoresMethod = "Bartlett")
  solution$mydata <- data.frame(factorz$scores)
  return(solution)

}
