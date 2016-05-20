#' @title  Creating the factor loading model
#' @description Inside the ML function this function creates a factor loading model, in order to reduce the ammount of dimensions from the original dataset. This function can be used seperately to generate a factor loading model for any given dataframe
#' @param mydata a dataframe containing the data to be analysed
#' @param analyticalVariables A vector containing the names of the variables in the dataset that should be analysed
#' @param selectedNumberOfDimensions the number of dimensions to be created, the number of produced factors. If 0, the number of dimensions is calculated by using the eigenvalues
#' @param removeCata A boolean indicating if categorical variables should be removed from the dataset
#' @param faRotate A string specifying the method to be used in rotation, default: oblimin
#' @param faMethodScores A string describing the method used for calculating the factors
#' @param pcMethod A string specifying the factoring method the be used
#' @param selectedCorrelationCutoff A number in range 0-1 specifying the maximum ammount of correlation between variables
#' @examples
#' require("data.table")
#' data <- fread("data.csv")
#' data <- data.frame(data)
#' createFactorLoadingModel(mydata=data, analyticalVariables=c("var1", "var2"), removeCata=T, pcMethod='wls', selectedCorrelationCutoff=0.90)
#' @export
createFactorLoadingModel <- function(mydata , analyticalVariables, selectedNumberOfDimensions,
                                    selectedDataReductionMethod,removeCata,
                                    faRotate,faMethodScores, pcMethod,  selectedCorrelationCutoff) {

  if(removeCata == TRUE){
  cordata=cor(mydata[,analyticalVariables], use="complete.obs", method="pearson")
  }else{
    cordata=cor(mydata[,analyticalVariables], use="complete.obs", method="spearman")
  }
  mydataMatrix <- as.matrix(mydata[,analyticalVariables])

  # reset the number of Dimensions, if selectedNumberOfDimensions equal to 0
  # the number of Dimensions is equel to the eigen value.
  selectedNumberOfDimensions <- if(selectedNumberOfDimensions==0) sum(as.numeric(eigen(
    as.matrix(cordata))$values>1)) else  selectedNumberOfDimensions


  covx <- rrcov::Cov(mydataMatrix)
  covList <- list(cov = rrcov::getCov(covx), center = rrcov::getCenter(covx))

  if (selectedDataReductionMethod == "FA") {
    print("factorAnalysis")

    factorList <- factorAnalysis(mydata = mydataMatrix,
                 selectedNumberOfDimensions = selectedNumberOfDimensions, covList=covList,faRotate=faRotate,
                 faMethodScores=faMethodScores, pcMethod=pcMethod)
    while(class(factorList)[1] == "try-error"){

      selectedCorrelationCutoff <- selectedCorrelationCutoff-0.01
      updateList <- eliminateHighestMissingVariableInCorrelationCutoff(mydata, analyticalVariables,
                                                                     selectedCorrelationCutoff, removeCata)
      analyticalVariables <- analyticalVariables [! analyticalVariables %in% updateList$updateVariables]
      mydataMatrix <- as.matrix(mydata[,analyticalVariables])
      covx <- rrcov::Cov(mydataMatrix)
      covList <- list(cov = rrcov::getCov(covx), center = rrcov::getCenter(covx))
      factorList <- try(factorAnalysis(mydata = mydataMatrix,
                                     selectedNumberOfDimensions = selectedNumberOfDimensions, covList=covList,faRotate=faRotate,
                                     faMethodScores=faMethodScores, pcMethod=pcMethod), silent = FALSE)

    }
  }
  else if (selectedDataReductionMethod == "PCA"){
    print("Principal component analysis")
    factorList <- try(Principalcomponentanalysis(mydata = mydataMatrix, selectedNumberOfDimensions, covList, faMethodScores=faMethodScores))
    while(class(factorList)[1] == "try-error"){
      selectedCorrelationCutoff <- selectedCorrelationCutoff - 0.01
      updateList <- eliminateHighestMissingVariableInCorrelationCutoff(mydata, analyticalVariables,
                                                                       selectedCorrelationCutoff, removeCata)
      analyticalVariables <- analyticalVariables [! analyticalVariables %in% updateList$updateVariables]
      mydataMatrix <- as.matrix(mydata[,analyticalVariables])
      covx <- rrcov::Cov(mydataMatrix)
      covList <- list(cov = rrcov::getCov(covx), center = rrcov::getCenter(covx))
      factorList <- try(Principalcomponentanalysis(mydata = mydataMatrix, selectedNumberOfDimensions=selectedNumberOfDimensions,
                                                   covList=covList, faMethodScores=faMethodScores))
    }
  }else{
    print("No data reduction applied")
  }
  # renameColNames is a fuction to make t new names for the coloms
  colnames(factorList$mydata) <- renameColNames(numberOfFactors = ncol(factorList$mydata))
  factorList$analyticalVariables <- colnames(factorList$mydata)

  return(factorList)
}
