#' @title  Removing the variables containing the most NA values
#' @description This function is used internally inside the function; ExcludedVariablesModel to remove the variables from the dataset containing to many NA values.
#' @param mydata a dataframe containing the data to be analysed
#' @param analyticalVariables A vector containing the names of the variables in the dataset that should be analysed
#' @param selectedCorrelationCutoff A number in range 0-1 specifying the maximum ammount of correlation between variables
#' @param removeCata A boolean indicating if categorical variables should be removed from the dataset
#' @param corData A matrix containing information about the ammount of correlation between variables, if NULL, the function calculates this by itsself.


eliminateHighestMissingVariableInCorrelationCutoff <- function(mydata, analyticalVariables,
                                                               selectedCorrelationCutoff,removeCata,
                                                               corData = NULL ){
  if(is.null(corData)){
    if(removeCata == TRUE){
      corData=cor(mydata[,analyticalVariables], use="complete.obs", method="pearson")
    }else{
      corData=cor(mydata[,analyticalVariables], use="complete.obs", method="spearman")
    }
  }
  updateVariables <-c()
  listRm <- sapply(analyticalVariables,
                   correlationCutoffFilter, corData, selectedCorrelationCutoff)
  for (i in names(listRm[!sapply(listRm, is.null)])){
    var <- correlationByMissing(i, listRm[!sapply(listRm, is.null)], mydata, analyticalVariables)
    updateVariables <- append(updateVariables,var)

  }
  returnList <- list()
  returnList$updateVariables <- updateVariables
  returnList$corData <- corData
  return(returnList)




}
