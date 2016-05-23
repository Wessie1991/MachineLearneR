#' @title  Function for finding NA values
#' @description This function is used internally inside the function; \code{\link{eliminateHighestMissingVariableInCorrelationCutoff}} to find the NA values per variable
#' @param x a string specifying the name of the current variable under investigation
#' @param listRM a list containing the variables to be removed from the dataset
#' @param mydata a dataframe containing the data to be analysed
#' @param analyticalVariables a vector containing the names of the variables to be analysed
#' @export
correlationByMissing <- function(x, listRm, mydata, analyticalVariables){
  varMostNAs = c()
  varNAs <- apply(mydata[,listRm[[x]]], 2, function(x) sum(is.na(x)))
  varMostNAs <- (sort(varNAs,decreasing = TRUE)[1:(length(varNAs)-1)])

  return(names(varMostNAs))

}
