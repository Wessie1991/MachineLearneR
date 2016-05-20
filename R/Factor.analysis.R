factorAnalysis <- function(mydata,selectedNumberOfDimensions, covList, faRotate, faMethodScores, pcMethod ){

  solution <- psych::fa(r=cor(mydata),
              nfactors=selectedNumberOfDimensions, rotate=faRotate, SMC=FALSE, fm=pcMethod)

  solution$loadings <- as.matrix(solution$loadings[,order(colnames(solution$loadings))])
  # aply factor Analysis on mydata.
  factorz <- robustfa::computeScores(solution, x = mydata,
                          covmat = covList, cor = TRUE, scoresMethod = faMethodScores)

  solution$mydata <- data.frame(factorz$scores)
  return(solution)

}
