#' @title  Applying factor analysis
#' @description Inside the \code{\link{createFactorLoadingModel}} function this function is used to perform factor analysis on a given data frame.
#' @param mydata a dataframe containing the data to be analysed
#' @param selectedNumberOfDimensions the number of dimensions to be created, the number of produced factors. If 0, the number of dimensions is calculated by using the eigenvalues
#' @param covList A list with the components cov, center, and n.obs.
#' @param faRotate A string specifying the method to be used in rotation, default: oblimin
#' @param faMethodScores A string describing the method used for calculating the factors
#' @param pcMethod A string specifying the factoring method the be used.
#' @export
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
